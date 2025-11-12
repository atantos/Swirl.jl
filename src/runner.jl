# runner.jl — lesson runners + REPL handler (with lesson navigation and course selection)

# Markdown-aware show helper
_show(x::AbstractString) = println(x)
_show(x::Markdown.MD) = display(x)


"""
    run_question_setup(question)

Run setup code for a question if it exists. This ensures required variables
from previous questions are available even if the user restarted Julia.
"""
function run_question_setup(question::Question)
    if !isempty(question.setup)
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

"""
    run_lesson_classic_mode(course_name, lesson)

Run an interactive lesson using classic readline mode (original implementation).
"""
function run_lesson_classic_mode(course_name::String, lesson::Lesson)
    println("\n" * "="^60)
    println("| $(lesson.name)")
    println("="^60)
    println(lesson.description)
    println()

    progress = get_lesson_progress(course_name, lesson.name)

    # Start from saved progress or beginning
    start_idx = progress.completed ? 1 : progress.current_question

    if progress.completed
        println("You've already completed this lesson!")
        print("Do you want to restart? (yes/no): ")
        response = lowercase(strip(readline()))
        if response != "yes" && response != "y"
            println("\n👋 Returning to lesson menu...")
            return :no_restart
        end
        progress = LessonProgress(course_name, lesson.name)
        start_idx = 1
    end

    # Run through questions
    for (idx, question) in enumerate(lesson.questions[start_idx:end])
        actual_idx = start_idx + idx - 1

        if !run_question_classic(question, actual_idx, length(lesson.questions))
            # User wants to exit
            progress.current_question = actual_idx
            save_lesson_progress(progress)
            println("\nProgress saved. Type swirl() to continue later!")
            return
        end

        progress.correct_answers += 1
        progress.current_question = actual_idx + 1
        save_lesson_progress(progress)
    end

    # Lesson completed!
    progress.completed = true
    save_lesson_progress(progress)

    println("\n" * "="^60)
    println("| 🎉 Congratulations!")
    println("="^60)
    println("You've completed $(lesson.name)!")
    total_questions = count(q -> q.type != :message, lesson.questions)
    println("Score: $(progress.correct_answers)/$total_questions")
    println()
    return :completed
end

"""
    run_question_classic(question, idx, total)

Run a single question in classic mode.
"""
function run_question_classic(question::Question, idx::Int, total::Int)
    println("\n--- Question $idx of $total ---")
    println()

    if question.type == :message
        _show(question.text)
        println()
        print("Press Enter to continue...")
        readline()
        return true
    end

    # Handle multistep questions differently
    if question.type == :multistep_code
        return run_multistep_question_classic(question, idx, total)
    end

    # Run setup code before displaying the question
    run_question_setup(question)

    _show(question.text)
    println()

    if question.type == :multiple_choice && !isempty(question.choices)
        for (i, choice) in enumerate(question.choices)
            print("  $i. ")
            _show(choice)
        end
        println()
    end

    max_attempts = 3
    attempts = 0

    while attempts < max_attempts
        attempts += 1

        if question.type == :code
            print("julia> ")
        else
            print("Your answer: ")
        end

        user_input = readline()

        if lowercase(strip(user_input)) in ["exit", "quit", "bye"]
            return false
        elseif lowercase(strip(user_input)) in ["skip"]
            println("⏭ Skipping this question...")
            return true
        elseif lowercase(strip(user_input)) in ["hint", "help"]
            if !isempty(String(question.hint)) || (question.hint isa Markdown.MD)
                print("💡 Hint: ")
                _show(question.hint)
            else
                println("💡 No hint available for this question.")
            end
            attempts -= 1
            continue
        end

        result = check_answer(user_input, question.answer, question.type)

        if question.type == :code && result isa NamedTuple
            if result.correct
                println("✓ Correct! $(question.type == :code ? "Great work!" : "")")
                println()
                return true
            else
                println("✗ $(result.message)")
                if attempts < max_attempts
                    println("Try again (attempt $(attempts+1)/$max_attempts, or type 'hint' for help):")
                end
            end
        elseif result == true || (result isa Bool && result)
            println("✓ Correct!")
            println()
            return true
        else
            println("✗ Not quite right.")
            if attempts < max_attempts
                println("Try again (attempt $(attempts+1)/$max_attempts, or type 'hint' for help):")
            end
        end
    end

    println("\n⏭ The correct answer was: $(question.answer)")
    if !isempty(question.hint)
        println("💡 $(question.hint)")
    end
    println()

    return true
end

"""
    run_multistep_question_classic(question, idx, total)

Run a multistep question in classic mode, where user provides code line by line.
"""
function run_multistep_question_classic(question::Question, idx::Int, total::Int)
    _show(question.text)
    println()

    # Run setup code before starting multistep
    run_question_setup(question)

    println("This is a multi-step question. Enter your code line by line.")
    println("   Type 'done' when finished, 'hint' for help, or 'skip' to skip.")
    println()

    current_step = 1
    max_steps = question.required_steps
    code_lines = String[]

    while current_step <= max_steps
        # Show step prompt if available
        if current_step <= length(question.steps) && !isempty(question.steps[current_step])
            println("Step $current_step/$max_steps: $(question.steps[current_step])")
        else
            println("Step $current_step/$max_steps:")
        end

        print("julia> ")
        user_input = readline()

        # Handle commands
        low = lowercase(strip(user_input))
        if low in ["exit", "quit", "bye"]
            return false
        elseif low == "skip"
            println("Skipping this question...")
            return true
        elseif low == "done"
            # Check if enough steps completed
            if current_step <= max_steps
                println("Please complete all $max_steps steps. (You're on step $current_step)")
                continue
            end
            break
        elseif low in ["hint", "help"]
            # Show step-specific hint if available
            if current_step <= length(question.step_hints) && !isempty(question.step_hints[current_step])
                println("Hint: $(question.step_hints[current_step])")
            elseif !isempty(question.hint)
                println("Hint: $(question.hint)")
            else
                println("No hint available for this step.")
            end
            continue
        elseif isempty(strip(user_input))
            println("Please enter some code.")
            continue
        end

        # Execute the code
        push!(code_lines, user_input)
        eval_result = safe_eval(user_input)

        if !eval_result.success
            println("Error: $(eval_result.error)")
            println("Try again, or type 'hint' for help.")
            pop!(code_lines)  # Remove failed code
            continue
        end

        # Show result (like REPL) - but suppress 'nothing'
        if eval_result.result !== nothing
            println(eval_result.result)
        end

        current_step += 1
        println()
    end

    # Check final answer if all steps completed
    if current_step > max_steps
        # Execute all code again to get final result
        full_code = join(code_lines, "\n")
        final_result = safe_eval(full_code)

        if final_result.success
            if question.answer === nothing || final_result.result == question.answer
                println("Correct! You've completed all steps successfully!")
                println()
                return true
            else
                println("All steps executed, but the final result doesn't match expected.")
                println("Your result: $(final_result.result)")
                println("Expected: $(question.answer)")
                println()
                return true  # Still advance
            end
        else
            println("Error in final evaluation: $(final_result.error)")
            return true  # Still advance
        end
    end

    return true
end

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

"""
    display_question(state)
"""
function display_question(state::ReplLessonState)
    if state.current_question_idx > length(state.lesson.questions)
        return
    end

    question = state.lesson.questions[state.current_question_idx]

    println("\n--- Question $(state.current_question_idx) of $(length(state.lesson.questions)) ---")
    println()

    if question.type == :message
        _show(question.text)
        println()
        # Automatically advance after message - no need to wait for input
        state.waiting_for_message = false

        # Check if this is the last question and complete if so
        if state.current_question_idx >= length(state.lesson.questions)
            state.current_question_idx += 1
            state.progress.current_question = state.current_question_idx
            save_lesson_progress(state.progress)

            state.progress.completed = true
            save_lesson_progress(state.progress)
            state.lesson_complete = true

            println("\n" * "="^60)
            println("| 🎉 Congratulations!")
            println("="^60)
            println("You've completed $(state.lesson.name)!")
            total_questions = count(q -> q.type != :message, state.lesson.questions)
            println("Score: $(state.progress.correct_answers)/$total_questions")
            println()

            display_lesson_menu(state)
        end
    else
        # Run setup code before displaying the question
        run_question_setup(question)

        _show(question.text)
        println()

        if question.type == :multiple_choice && !isempty(question.choices)
            for (i, choice) in enumerate(question.choices)
                print("  $i. ")
                _show(choice)
            end
            println()
        end

        # For multistep questions, show current step
        if question.type == :multistep_code
            println("Multi-step question - Enter code line by line.")
            println("   Type 'done' when finished, 'hint' for help, 'skip' to skip.")
            if state.multistep_current_step <= question.required_steps
                # Show step-specific prompt if available
                if state.multistep_current_step <= length(question.steps) &&
                   !isempty(question.steps[state.multistep_current_step])
                    println()
                    println("Step $(state.multistep_current_step) of $(question.required_steps):")
                    _show(question.steps[state.multistep_current_step])
                else
                    println()
                    println("Step $(state.multistep_current_step) of $(question.required_steps):")
                    _show(question.steps[state.multistep_current_step])
                end
            end
            println()
        end

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
        current = (i == state.current_lesson_idx) ? " ← just completed" : ""
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
                last_completed_idx = 0
                for (i, lesson) in enumerate(selected_course.lessons)
                    progress = get_lesson_progress(selected_course.name, lesson.name)
                    if progress.completed
                        last_completed_idx = i  # Track the highest completed lesson index
                    end
                end

                lesson_state = ReplLessonState(
                    selected_course,
                    last_completed_idx,
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

function handle_hint(state::ReplLessonState)
    q = state.lesson.questions[state.current_question_idx]

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
end

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
    state.lesson_complete = false
    display_question(state)
end

function handle_exit(state::ReplLessonState)
    state.progress.current_question = state.current_question_idx
    save_lesson_progress(state.progress)
    println("\n💾 Progress saved!")
    println("👋 Press backspace to exit Swirl mode. Run swirl() to continue later.")
    state.lesson_complete = true
    CURRENT_LESSON_STATE[] = nothing
end

function process_answer(state::ReplLessonState, input::AbstractString)
    input = String(input)
    q = state.lesson.questions[state.current_question_idx]

    # Handle multistep questions
    if q.type == :multistep_code
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
                    println("Correct! You've completed all steps successfully!")
                    println()
                    state.progress.correct_answers += 1
                    # Reset multistep state
                    state.multistep_current_step = 1
                    state.multistep_code_lines = String[]
                    advance_to_next_question(state)
                else
                    println("All steps executed, but result doesn't match expected.")
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
            println("Error: $(eval_result.error)")
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

        if state.multistep_current_step <= q.required_steps
            # Show next step prompt
            println()
            if state.multistep_current_step <= length(q.steps) &&
               !isempty(q.steps[state.multistep_current_step])
                println("Step $(state.multistep_current_step) of $(q.required_steps):")
                _show(q.steps[state.multistep_current_step])
            else
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
end

function handle_incorrect_answer(state::ReplLessonState)
    if state.current_attempts < state.max_attempts
        rem = state.max_attempts - state.current_attempts
        println("Try again (attempt $(state.current_attempts+1)/$(state.max_attempts), or type 'hint' for help):")
    else
        q = state.lesson.questions[state.current_question_idx]
        println("\n⏭ The correct answer was: $(q.answer)")
        if !isempty(q.hint)
            println("💡 $(q.hint)")
        end
        println()
        advance_to_next_question(state)
    end
end

function advance_to_next_question(state::ReplLessonState)
    # Reset multistep state when advancing
    state.multistep_current_step = 1
    state.multistep_code_lines = String[]
    state.current_attempts = 0

    state.current_question_idx += 1
    state.progress.current_question = state.current_question_idx
    save_lesson_progress(state.progress)

    if state.current_question_idx > length(state.lesson.questions)
        # Lesson completed!
        state.progress.completed = true
        save_lesson_progress(state.progress)
        state.lesson_complete = true

        println("\n" * "="^60)
        println("| Congratulations!")
        println("="^60)
        println("You've completed $(state.lesson.name)!")
        total_questions = count(q -> q.type != :message, state.lesson.questions)
        println("Score: $(state.progress.correct_answers)/$total_questions")
        println()

        # Automatically show the menu
        display_lesson_menu(state)
    else
        display_question(state)
    end
end

# Backward-compatibility aliases
const run_lesson = run_lesson_classic_mode
const run_question = run_question_classic
