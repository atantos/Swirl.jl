# runner.jl — lesson runners + REPL handler (with lesson navigation and course selection)

"""
    run_question_setup(question)

Run setup code for a question if it exists. This ensures required variables
from previous questions are available even if the user restarted Julia.
"""
function run_question_setup(question::AbstractQuestion)
    if hasproperty(question, :setup) && !isempty(question.setup)
        eval_result = safe_eval(question.setup)
        if !eval_result.success
            @warn "Setup code failed: $(eval_result.error)"
            println("Note: Some variables from previous questions may not be available.")
        end
    end
end

# ============================================================================
# CLASSIC MODE (readline-based)
# ============================================================================
# move classic case to
include("runner_classic.jl")


# ============================================================================
# REPL MODE (ReplMaker-based with lesson and course navigation)
# ============================================================================

mutable struct ReplLessonState
    course::Course
    current_lesson_idx::Int
    lesson::Lesson
    progress::LessonProgress
    current_question_idx::Int
    current_attempts::Int
    max_attempts::Int
    waiting_for_message::Bool
    waiting_for_menu_choice::Bool
    lesson_complete::Bool
    waiting_for_restart_confirmation::Bool
    was_previously_completed::Bool
    waiting_for_reset_all_confirmation::Bool
    multistep_current_step::Int  # NEW: For multistep questions
    multistep_code_lines::Vector{String}  # NEW: Accumulated code for
end

mutable struct ReplCourseState
    courses::Vector{Course}
    waiting_for_course_choice::Bool
end

const CURRENT_LESSON_STATE = Ref{Any}(nothing)
const REPL_MODE_INITIALIZED = Ref(false)

"""
    run_lesson_repl_mode(course_name, lesson)

Run an interactive lesson using ReplMaker REPL mode with syntax highlighting.
"""
function run_lesson_repl_mode(course_name::String, lesson::Lesson)
    # Get the full course to enable navigation
    course = nothing
    for c in get_available_courses()
        if c.name == course_name
            course = c
            break
        end
    end

    if course === nothing
        error("Course not found: $course_name")
    end

    # Find lesson index
    lesson_idx = findfirst(l -> l.name == lesson.name, course.lessons)
    if lesson_idx === nothing
        error("Lesson not found in course")
    end

    # println("\n" * "="^60)
    # println("| $(lesson.name)")
    # println("="^60)
    _show(lesson.title)
    _show(lesson.description)
    println()

    progress = get_lesson_progress(course_name, lesson.name)
    start_idx = progress.completed ? 1 : progress.current_question

    if progress.completed
        state = ReplLessonState(
            course,
            lesson_idx,
            lesson,
            progress,
            start_idx,
            0,
            3,
            false,
            false,
            false,
            true,  # waiting_for_restart_confirmation
            true,  # was_previously_completed
            false,  # waiting_for_reset_all_confirmation
            1,              # multistep_current_step
            String[]        # multistep_code_lines
        )
        CURRENT_LESSON_STATE[] = state

        println("You've already completed this lesson!")
        println("Restart it? (Type 'yes' or 'no')")
        println()
        return true
    end

    # Initialize lesson state with course info
    state = ReplLessonState(
        course,
        lesson_idx,
        lesson,
        progress,
        start_idx,
        0,      # current_attempts
        3,      # max_attempts
        false,  # waiting_for_message
        false,  # waiting_for_menu_choice
        false,  # lesson_complete
        false,  # waiting_for_restart_confirmation
        false,  # was_previously_completed
        false,   # waiting_for_reset_all_confirmation
        1,              # multistep_current_step
        String[]        # multistep_code_lines
    )
    CURRENT_LESSON_STATE[] = state

    display_question(state)

    return true
end

# Announce which question if something to ask
function display_lesson_progress(q::AbstractQuestion, state)
    isaquestion(q) || return nothing

    # Count questions BEFORE current one, then add 1 for current question number
    m = if state.current_question_idx == 1
        1
    else
        count(isaquestion, state.lesson.questions[1:state.current_question_idx-1]) + 1
    end
    N = count(isaquestion, state.lesson.questions)

    println("\n--- Question $m of $N ---")
    println()
end
display_lesson_progress(q::OutputOnly, state) = nothing

"""
    display_question(state)
"""
function display_question(state::ReplLessonState)
    if state.current_question_idx > length(state.lesson.questions)
        return
    end


    question = state.lesson.questions[state.current_question_idx]

    # Announce which question if something to ask
    display_lesson_progress(question, state)

    if isa(question, OutputOnly)
        show_question(question)
        println()

        # Auto-advance to next question (I changed that to be consistent wrt the advancement)
        advance_to_next_question(state)
        return  # Important: return here to avoid showing next question twice

    elseif isa(question, MultistepCodeQ)
        # Initialize or resume multistep state
        if !haskey(state.progress.multistep_state, state.current_question_idx)
            state.multistep_current_step = 1
            state.multistep_code_lines = String[]
            state.progress.multistep_state[state.current_question_idx] = 1
        else
            state.multistep_current_step = state.progress.multistep_state[state.current_question_idx]
            # Note: Can't restore code_lines from previous session
            state.multistep_code_lines = String[]
        end

        # Show initial question text on first step
        if state.multistep_current_step == 1
            run_question_setup(question)
            show_question(question)
        end
        println("\n🔢 Multi-step question - Enter code line by line.")
        println("   Commands:")
        println("   • 'done'             - Submit your code")
        println("   • 'hint'             - Get help for current step")
        println("   • 'skip'             - Skip this question")
        println("   • 'restart question' - Restart from step 1")
        println("   • 'menu'             - Return to lesson menu")
        println()
        if state.multistep_current_step <= question.required_steps
            # Show step-specific prompt if available
            if state.multistep_current_step <= length(question.steps) &&
               !isempty(question.steps[state.multistep_current_step])
                println()
                println("Step $(state.multistep_current_step) of $(question.required_steps):")
                _show(question.steps[state.multistep_current_step])
            end
        end
        println()

    else

        # Run setup code before displaying the question
        run_question_setup(question)
        show_question(question)
        println()

        state.waiting_for_message = false
    end

    state.current_attempts = 0
end

"""
    display_lesson_menu(state)
"""
function display_lesson_menu(state::ReplLessonState)
    # Build lesson lines with progress + “current” marker
    lines = String[]
    for (i, lesson) in enumerate(state.course.lessons)
        progress = get_lesson_progress(state.course.name, lesson.name)
        status = progress.completed ? "✓" : " "

        # Show appropriate marker based on lesson state
        if i == state.current_lesson_idx
            if progress.completed
                current = " ← just completed"
            else
                current = " ← in progress"
            end
        else
            current = ""
        end

        push!(lines, "$i. [$status] $(lesson.name)$current")
    end
    body = """
# 📘 Lessons in **$(state.course.name)**

$(join(lines, "\n"))

---

### ⚙️ Commands
- `0` — Back to course selection
- `-1` — Exit Swirl
- `reset <number>` — Reset a specific lesson (e.g. `reset 1`)
- `reset all` — Reset all lessons in this course

💡 **Type a lesson number or command:**
"""
    _show(Markdown.parse(body))
    state.waiting_for_menu_choice = true
end



"""
    display_course_menu(courses)
"""
# function display_course_menu(courses::Vector{Course})
#     println("\nAvailable courses:")
#     for (i, course) in enumerate(courses)
#         println("  $i. $(course.name)")
#     end
#     println(" -1. Exit Swirl")
#     println()
#     println("💡 Select a course (enter number):")
# end
function display_course_menu(state::ReplCourseState)
    courses = state.courses

    course_lines = isempty(courses) ?
                   "_No courses installed yet._" :
                   join(["$(i). $(c.name)" for (i, c) in enumerate(courses)], "\n")

    body = """
# 🌀 Welcome to **Swirl for Julia!**

*Type `)` to enter Swirl mode.*
*(Press backspace anytime to exit Swirl mode.)*

## Available courses
$course_lines

---

### ⚙️ Commands
- `-1` — Exit Swirl

💡 **Select a course (enter number):**
"""
    display(Markdown.parse(body))
    state.waiting_for_course_choice = true
end


"""
    swirl_repl_handler(input)

Main input handler for Swirl's REPL mode (powered by ReplMaker.jl).
"""
function swirl_repl_handler(input::AbstractString)
    input = String(strip(input))
    state = CURRENT_LESSON_STATE[]

    # Handle course selection mode
    if state isa ReplCourseState
        if input == ""
            # Empty input in course selection, just wait
            return nothing
        end

        try
            choice = parse(Int, input)
            if choice == -1
                println("\n👋 Goodbye! Press backspace to exit Swirl mode and return to Julia.")
                CURRENT_LESSON_STATE[] = nothing
                return nothing
            elseif choice >= 1 && choice <= length(state.courses)
                selected_course = state.courses[choice]
                # Show lesson menu for selected course
                # Create a temporary lesson state for menu display
                first_lesson = selected_course.lessons[1]

                # Find the most recently completed lesson to show the indicator correctly
                last_lesson_idx = 0
                for (i, lesson) in enumerate(selected_course.lessons)
                    progress = get_lesson_progress(selected_course.name, lesson.name)
                    # Track if lesson is completed OR has been started (current_question > 1)
                    if progress.completed || progress.current_question > 1
                        last_lesson_idx = i
                    end
                end

                lesson_state = ReplLessonState(
                    selected_course,
                    last_lesson_idx,
                    first_lesson,
                    LessonProgress(selected_course.name, first_lesson.name),
                    1,
                    0,
                    3,
                    false,
                    true,  # waiting_for_menu_choice
                    false,
                    false,
                    false,
                    false,  # waiting_for_reset_all_confirmation
                    1,              # multistep_current_step
                    String[]        # multistep_code_lines
                )
                CURRENT_LESSON_STATE[] = lesson_state
                display_lesson_menu(lesson_state)
                return nothing
            else
                println("Invalid choice. Enter a number between -1 and $(length(state.courses))")
            end
        catch
            println("Invalid input. Please enter a number.")
        end
        return nothing
    end

    # Rest of handler for lesson state
    if state === nothing
        # State cleared, silently wait for user to exit
        return nothing
    end
    @assert state isa ReplLessonState

    # Handle restart confirmation
    if state.waiting_for_restart_confirmation
        response = lowercase(strip(input))
        if response == "yes" || response == "y"
            # Reset progress and start from beginning
            state.progress = LessonProgress(state.course.name, state.lesson.name)
            state.current_question_idx = 1
            state.was_previously_completed = false  # Clear completion flag when restarting
            state.waiting_for_restart_confirmation = false

            # println("\n" * "="^60)
            # println("| $(state.lesson.name)")
            # println("="^60)
            _show(state.lesson.title)
            _show(state.lesson.description)
            println()

            display_question(state)
        elseif response == "no" || response == "n"
            println("\n👋 Returning to lesson menu...")
            state.waiting_for_restart_confirmation = false
            state.waiting_for_menu_choice = true
            display_lesson_menu(state)
        else
            println("Please type 'yes' or 'no'")
        end
        return nothing
    end

    # Handle reset all confirmation
    if state.waiting_for_reset_all_confirmation
        response = lowercase(strip(input))
        if response == "yes" || response == "y"
            count = 0
            for lesson in state.course.lessons
                progress_file = get_progress_file(state.course.name, lesson.name)
                if isfile(progress_file)
                    rm(progress_file)
                    count += 1
                end
            end
            println("✓ Reset $count lesson$(count == 1 ? "" : "s")")
            state.waiting_for_reset_all_confirmation = false
            state.waiting_for_menu_choice = true
            display_lesson_menu(state)
        else
            println("Cancelled.")
            state.waiting_for_reset_all_confirmation = false
            state.waiting_for_menu_choice = true
            display_lesson_menu(state)
        end
        return nothing
    end

    # Message-type questions - check this EARLY
    if state.waiting_for_message
        advance_to_next_question(state)
        return nothing
    end

    # Display first question when entering REPL mode with empty input
    if input == "" && !state.waiting_for_menu_choice && !state.lesson_complete
        display_question(state)
        return nothing
    end

    # Handle lesson menu selection
    if state.waiting_for_menu_choice
        # Check for reset commands first (before trying to parse as number)
        if startswith(lowercase(input), "reset all")
            handle_reset_all_repl(state)
            return nothing
        elseif startswith(lowercase(input), "reset ")
            handle_reset_lesson_repl(state, input)
            return nothing
        end

        try
            choice = parse(Int, input)
            if choice == -1
                println("\n👋 Goodbye! Press backspace to exit Swirl mode and return to Julia.")
                CURRENT_LESSON_STATE[] = nothing
                return nothing
            elseif choice == 0
                # Go back to course selection
                courses = get_available_courses()
                course_state = ReplCourseState(courses, true)
                CURRENT_LESSON_STATE[] = course_state
                display_course_menu(course_state)
                return nothing
            elseif choice >= 1 && choice <= length(state.course.lessons)
                # Load new lesson
                new_lesson = state.course.lessons[choice]
                new_progress = get_lesson_progress(state.course.name, new_lesson.name)

                # Update state for new lesson
                state.current_lesson_idx = choice
                state.lesson = new_lesson
                state.progress = new_progress
                state.current_question_idx = new_progress.completed ? 1 : new_progress.current_question
                state.lesson_complete = false
                state.waiting_for_menu_choice = false
                state.was_previously_completed = new_progress.completed

                if new_progress.completed
                    println("\nYou've already completed this lesson!")
                    println("Restart it? (yes/no): ")
                    state.waiting_for_restart_confirmation = true
                    return nothing
                end

                # println("\n" * "="^60)
                # println("| $(new_lesson.name)")
                # println("="^60)
                _show(new_lesson.title)
                _show(new_lesson.description)
                println()

                display_question(state)
            else
                println("Invalid choice. Enter a number between -1 and $(length(state.course.lessons))")
            end
        catch
            println("Invalid input. Please enter a number or command (e.g., 'reset 1').")
        end
        return nothing
    end

    if state.lesson_complete
        if lowercase(input) in ["exit", "quit", "bye"]
            println("\n👋 Goodbye! Press backspace to exit Swirl mode and return to Julia.")
            CURRENT_LESSON_STATE[] = nothing
        end
        return nothing
    end

    # Commands
    low = lowercase(input)
    if low == "hint" || low == "help"
        handle_hint(state)
        return nothing
    end
    if low == "skip"
        handle_skip(state)
        return nothing
    end
    if low == "restart"
        handle_restart(state)
        return nothing
    end
    # Added support for 'restart question' command
    if low == "restart question" || low == "rq"
        handle_restart_question(state)
        return nothing
    end
    if low in ["exit", "quit", "bye"]
        handle_exit(state)
        return nothing
    end
    if low == "menu"
        println("\n💾 Saving progress and returning to lesson menu...")
        state.progress.current_question = state.current_question_idx

        # If this lesson was previously completed, restore that status
        if state.was_previously_completed
            state.progress.completed = true
        end

        save_lesson_progress(state.progress)
        state.lesson_complete = false
        state.waiting_for_menu_choice = true
        display_lesson_menu(state)
        return nothing
    end

    # Process answer
    process_answer(state, input)
    return nothing
end

"""
    handle_hint(state)

Display hint for the current question using dispatch-based hint system.
"""
function handle_hint(state::ReplLessonState)
    q = state.lesson.questions[state.current_question_idx]
    if isa(q, MultistepCodeQ)
        _show_hint(q, state)  # ← Pass state for multistep
    else
        _show_hint(q)  # ← Don't pass state for others
    end
end
#=
# For multistep questions, show step-specific hint
if q.type == :multistep_code
    step = state.multistep_current_step
    if step <= length(q.step_hints) && !isempty(q.step_hints[step])
        println("💡 Hint:")
        _show(q.step_hints[step])
    elseif !isempty(q.hint)
        println("💡 Hint:")
        _show(q.hint)
    else
        println("No hint available for this step.")
    end
else
    if !isempty(String(q.hint)) || (q.hint isa Markdown.MD)
        print("💡 Hint: ")
        _show(q.hint)
    else
        println("No hint available for this question.")
    end
end
=#


function handle_reset_lesson_repl(state::ReplLessonState, input::AbstractString)
    # Parse lesson number from "reset 1", "reset 2", etc.
    parts = split(input)
    if length(parts) >= 2
        try
            lesson_num = parse(Int, parts[2])
            if lesson_num >= 1 && lesson_num <= length(state.course.lessons)
                lesson = state.course.lessons[lesson_num]

                # Delete progress file
                progress_file = get_progress_file(state.course.name, lesson.name)
                if isfile(progress_file)
                    rm(progress_file)
                    println("✓ Reset lesson: $(lesson.name)")
                else
                    println("ℹ️  Lesson not started yet: $(lesson.name)")
                end
                # Refresh the menu display
                display_lesson_menu(state)
            else
                println("Invalid lesson number. Enter a number between 1 and $(length(state.course.lessons))")
            end
        catch
            println("Invalid format. Use: reset <number> (e.g., 'reset 1')")
        end
    else
        println("Invalid format. Use: reset <number> (e.g., 'reset 1')")
    end
end

function handle_reset_all_repl(state::ReplLessonState)
    println("Are you sure you want to reset ALL lessons in $(state.course.name)?")
    println("Type 'yes' to confirm or anything else to cancel:")
    state.waiting_for_reset_all_confirmation = true
    state.waiting_for_menu_choice = false
end

function handle_skip(state::ReplLessonState)
    println("⏭ Skipping this question...")
    advance_to_next_question(state)
end

function handle_restart(state::ReplLessonState)
    println("\n🔄 Restarting lesson...")
    state.current_question_idx = 1
    state.progress = LessonProgress(state.course.name, state.lesson.name)
    state.was_previously_completed = false
    state.lesson_complete = false
    display_question(state)
end

"""
    handle_restart_question(state)

Restart the current question (useful for multistep questions).
"""
function handle_restart_question(state::ReplLessonState)
    q = state.lesson.questions[state.current_question_idx]
    if isa(q, MultistepCodeQ)
        println("\n🔄 Restarting question from step 1...")
        state.multistep_current_step = 1
        state.multistep_code_lines = String[]
        state.current_attempts = 0
        state.progress.multistep_state[state.current_question_idx] = 1
        save_lesson_progress(state.progress)
        display_question(state)
    else
        println("This command is only available for multi-step questions.")
        println("Use 'skip' to move to the next question or 'restart' to restart the entire lesson.")
    end
end

function handle_exit(state::ReplLessonState)
    state.progress.current_question = state.current_question_idx
    save_lesson_progress(state.progress)
    println("\n💾 Progress saved!")
    println("👋 Press backspace to exit Swirl mode. Run swirl() to continue later.")
    state.lesson_complete = true
    CURRENT_LESSON_STATE[] = nothing
end

"""
    process_answer(state, input)

Process user's answer using dispatch-based check_answer.
"""
function process_answer(state::ReplLessonState, input::AbstractString)
    input = String(input)
    q = state.lesson.questions[state.current_question_idx]

    # Handle multistep questions
    if isa(q, MultistepCodeQ)
        # Check for 'done' command
        if lowercase(strip(input)) == "done"
            if state.multistep_current_step <= q.required_steps
                println("Please complete all $(q.required_steps) steps. (You're on step $(state.multistep_current_step))")
                return
            end

            # All steps completed, check final answer
            full_code = join(state.multistep_code_lines, "\n")
            final_result = safe_eval(full_code)

            if final_result.success
                if q.answer === nothing || final_result.result == q.answer
                    println("✓ Correct! You've completed all steps successfully!")
                    println()
                    state.progress.correct_answers += 1
                    # Reset multistep state
                    state.multistep_current_step = 1
                    state.multistep_code_lines = String[]
                    advance_to_next_question(state)
                else
                    println("✗ All steps executed, but result doesn't match expected.")
                    println("Your result: $(final_result.result)")
                    println("Expected: $(q.answer)")
                    println()
                    # Reset and move forward
                    state.multistep_current_step = 1
                    state.multistep_code_lines = String[]
                    advance_to_next_question(state)
                end
            else
                println("Error in final evaluation: $(final_result.error)")
                state.multistep_current_step = 1
                state.multistep_code_lines = String[]
                advance_to_next_question(state)
            end
            return
        end

        # Execute the code line
        eval_result = safe_eval(input)

        if !eval_result.success
            println("✗ Error: $(eval_result.error)")
            println("Try again, or type 'hint' for help.")
            return
        end

        # Code executed successfully
        push!(state.multistep_code_lines, input)

        # Show result (like REPL) - suppress 'nothing'
        if eval_result.result !== nothing
            println(eval_result.result)
        end

        # Move to next step
        state.multistep_current_step += 1


        # Storing progress on a step level
        state.progress.multistep_state[state.current_question_idx] = state.multistep_current_step
        save_lesson_progress(state.progress)

        if state.multistep_current_step <= q.required_steps
            # Show next step prompt
            println()
            if state.multistep_current_step <= length(q.steps) &&
               !isempty(q.steps[state.multistep_current_step])
                println("Step $(state.multistep_current_step) of $(q.required_steps):")
                _show(q.steps[state.multistep_current_step])
            end
        else
            println()
            println("All steps entered! Type 'done' to finish, or continue entering code.")
        end

        return
    end


    # Regular questions
    state.current_attempts += 1

    # For CodeQ questions, evaluate and show the result first (like REPL behavior)
    if isa(q, CodeQ)
        eval_result = safe_eval(input)  # Evaluate

        if !eval_result.success
            # Evaluation failed
            println("✗ Error: $(eval_result.error)")
            handle_incorrect_answer(state)
            return
        end

        # Show result (like REPL) - suppress 'nothing'
        if eval_result.result !== nothing
            println(eval_result.result)
        end

        # Check if result matches expected answer 
        expected_answer = q.answer
        if eval_result.result == expected_answer
            result = (correct=true, message="")
        elseif typeof(eval_result.result) == typeof(expected_answer)
            # Right type but wrong value
            result = (correct=false, message="Not quite. You got $(eval_result.result), but the expected answer is $(expected_answer)")
        else
            result = (correct=false, message="Your code produced $(eval_result.result) (type: $(typeof(eval_result.result)))")
        end

        res = result.correct
    else
        # For non-code questions, use dispatch-based check_answer
        result = check_answer(input, q)
        res = isa(result, NamedTuple) ? result.correct : result
    end

    if res == true
        if isaquestion(q)
            println()
            println("✓ Correct!")
            println()
            state.progress.correct_answers += 1
        end
        advance_to_next_question(state)
    else
        if isaquestion(q)
            # Show appropriate error message
            if isa(result, NamedTuple)
                println("✗ $(result.message)")
            else
                println("✗ Not quite right.")
            end
        end
        handle_incorrect_answer(state)
    end
end
#=
# Regular questions
state.current_attempts += 1


result = check_answer(input, q.answer, q.type)

if q.type == :code && result isa NamedTuple
    if result.correct
        println("Correct! $(q.type == :code ? "Great work!" : "")")
        println()
        state.progress.correct_answers += 1
        advance_to_next_question(state)
    else
        println("$(result.message)")
        handle_incorrect_answer(state)
    end
elseif result == true || (result isa Bool && result)
    println("Correct!")
    println()
    state.progress.correct_answers += 1
    advance_to_next_question(state)
else
    println("Not quite right.")
    handle_incorrect_answer(state)
end
=#



"""
    handle_incorrect_answer(state)

Handle an incorrect answer - either allow retry or show correct answer.
"""
function handle_incorrect_answer(state::ReplLessonState)
    if state.current_attempts < state.max_attempts
        rem = state.max_attempts - state.current_attempts
        println("Try again (attempt $(state.current_attempts+1)/$(state.max_attempts), or type 'hint' for help):")
    else
        q = state.lesson.questions[state.current_question_idx]
        println("\n⏭ The correct answer was: $(q.answer)")
        # Show hint using dispatch
        if isa(q, MultistepCodeQ)
            _show_hint(q, state)
        else
            _show_hint(q)
            println()
            advance_to_next_question(state)
        end
    end
end

function advance_to_next_question(state::ReplLessonState)
    # Save multistep state before advancing
    if state.multistep_current_step > 1
        state.progress.multistep_state[state.current_question_idx] = state.multistep_current_step
    end

    # Reset multistep state when advancing
    state.multistep_current_step = 1
    state.multistep_code_lines = String[]
    state.current_attempts = 0

    state.current_question_idx += 1
    state.progress.current_question = state.current_question_idx
    save_lesson_progress(state.progress)

    if state.current_question_idx > length(state.lesson.questions)
        # Lesson completed!
        lesson_complete_summary(state)

        # Automatically show the menu
        display_lesson_menu(state)
    else
        display_question(state)
    end
end

function lesson_complete_summary(state)
    state.progress.completed = true
    save_lesson_progress(state.progress)
    state.lesson_complete = true

    println("\n" * "="^60)
    println("| Congratulations!")
    println("="^60)
    println("You've completed $(state.lesson.name)!")

    # Count only non-OutputOnly questions for scoring
    total_questions = count(q -> !isa(q, OutputOnly), state.lesson.questions)

    println("Score: $(state.progress.correct_answers)/$total_questions")
    println()
end

# Backward-compatibility aliases
const run_lesson = run_lesson_classic_mode
const run_question = run_question_classic
