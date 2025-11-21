module Swirl

using REPL
using Markdown
using Serialization
using ReplMaker

export swirl, install_course, uninstall_course, list_courses, delete_progress

# Core types
include("utils.jl")
include("types.jl")
include("progress.jl")
include("parser.jl")
include("question_types.jl")
include("runner.jl")
include("courses.jl")

# Try to load ReplMaker, but don't fail if it's not available
const REPLMAKER_AVAILABLE = Ref(false)
function __init__()
    try
        @eval using ReplMaker
        REPLMAKER_AVAILABLE[] = true
    catch
        REPLMAKER_AVAILABLE[] = false
    end
end

"""
    swirl(; use_repl_mode=:auto)

Start an interactive Swirl learning session with full navigation.
"""
function swirl(; use_repl_mode::Symbol=:auto)
    courses = get_available_courses()

    if isempty(courses)
        println("No courses installed yet!")
        println("The basic 'Julia Basics' course will be installed for you.")
        install_default_course()
        courses = get_available_courses()
    end

    # Determine if we should use REPL mode
    use_repl = if use_repl_mode == :auto
        REPLMAKER_AVAILABLE[]
    elseif use_repl_mode == :repl
        if !REPLMAKER_AVAILABLE[]
            error("ReplMaker mode requested but ReplMaker.jl is not available. Install with: using Pkg; Pkg.add(\"ReplMaker\")")
        end
        true
    else  # :classic
        false
    end

    if use_repl
        # Initialize REPL mode
        if !REPL_MODE_INITIALIZED[]
            ReplMaker.initrepl(
                swirl_repl_handler;
                repl=Base.active_repl,
                prompt_text="swirl> ",
                prompt_color=:cyan,
                start_key=')',
                mode_name="Swirl",
                sticky_mode=true,
                startup_text=true,
                completion_provider=REPL.REPLCompletionProvider(),
            )
            REPL_MODE_INITIALIZED[] = true
        end

        # Set up course selection state
        course_state = ReplCourseState(courses, true)
        CURRENT_LESSON_STATE[] = course_state

        # Display course menu
        display_course_menu(course_state)

        return
    else
        # Classic (alternative) mode
        println("\n" * "="^60)
        println("| Welcome to Swirl for Julia! 🌀")
        println("="^60)
        println()

        while true
            selected_course = course_selection_menu(courses)
            if selected_course === nothing
                println("\n👋 Thanks for using Swirl! Happy coding!")
                return
            end
            repl_launched = lesson_selection_loop(selected_course, use_repl_mode)
            if repl_launched
                return
            end
        end
    end
end

"""
    course_selection_menu(courses)

Display course selection menu and return selected course or nothing to exit.
"""
function course_selection_menu(courses)
    println("\nAvailable courses:")
    for (i, course) in enumerate(courses)
        println("  $i. $(course.name)")
    end
    println("  0. Exit Swirl")
    println()

    while true
        print("Select a course (enter number, or 0 to exit): ")
        input = strip(readline())

        if input == "0"
            return nothing
        end

        try
            choice = parse(Int, input)
            if choice >= 1 && choice <= length(courses)
                return courses[choice]
            else
                println("Please enter a number between 0 and $(length(courses))")
            end
        catch
            println("Invalid input. Please enter a number.")
        end
    end
end

"""
    lesson_selection_loop(course, use_repl_mode)

Handle lesson selection, reset commands, and navigation within a course.
"""
function lesson_selection_loop(course, use_repl_mode)
    while true
        # Display lesson list
        println("\n" * "="^60)
        println("Lessons in $(course.name):")
        println("="^60)
        for (i, lesson) in enumerate(course.lessons)
            progress = get_lesson_progress(course.name, lesson.name)
            status = progress.completed ? "✓" : " "
            println("  $i. [$status] $(lesson.name)")
        end
        println()
        println("Commands:")
        println("  0. Back to course selection")
        println("  reset <number>. Reset/retake a specific lesson (e.g., 'reset 1')")
        println("  reset all. Reset all lessons in this course")
        println()

        print("Select a lesson (enter number, command, or 0 to go back): ")
        input = strip(readline())

        # Handle input
        if input == "0"
            return false # Go back to course selection
        elseif startswith(lowercase(input), "reset all")
            handle_reset_all(course)
            continue
        elseif startswith(lowercase(input), "reset ")
            handle_reset_lesson(course, input)
            continue
        end

        # Try to parse as lesson number
        try
            lesson_idx = parse(Int, input)
            if lesson_idx >= 1 && lesson_idx <= length(course.lessons)
                selected_lesson = course.lessons[lesson_idx]

                # Run the lesson
                launched_repl_mode = run_lesson_with_mode(course.name, selected_lesson, use_repl_mode)

                if launched_repl_mode
                    return true
                else
                    # After lesson completes, loop back to lesson selection
                    continue
                end
            else
                println("Please enter a number between 0 and $(length(course.lessons))")
            end
        catch
            println("Invalid input. Please enter a number or command.")
        end
    end
end

"""
    handle_reset_lesson(course, input)

Reset a specific lesson based on user input.
"""
function handle_reset_lesson(course, input)
    # Parse lesson number from "reset 1", "reset 2", etc.
    parts = split(input)
    if length(parts) >= 2
        try
            lesson_num = parse(Int, parts[2])
            if lesson_num >= 1 && lesson_num <= length(course.lessons)
                lesson = course.lessons[lesson_num]

                # Delete progress file
                progress_file = get_progress_file(course.name, lesson.name)
                if isfile(progress_file)
                    rm(progress_file)
                    println()
                    println("✓ Reset lesson: $(lesson.name)")
                else
                    println()
                    println("ℹ️  Lesson not started yet: $(lesson.name)")
                end
            else
                println("Invalid lesson number. Enter a number between 1 and $(length(course.lessons))")
            end
        catch
            println("Invalid format. Use: reset <number> (e.g., 'reset 1')")
        end
    else
        println("Invalid format. Use: reset <number> (e.g., 'reset 1')")
    end
end

"""
    handle_reset_all(course)

Reset all lessons in a course.
"""
function handle_reset_all(course)
    print("Are you sure you want to reset ALL lessons in $(course.name)? (yes/no): ")
    response = lowercase(strip(readline()))

    if response == "yes" || response == "y"
        count = 0
        for lesson in course.lessons
            progress_file = get_progress_file(course.name, lesson.name)
            if isfile(progress_file)
                rm(progress_file)
                count += 1
            end
        end
        println("✓ Reset $count lesson$(count == 1 ? "" : "s")")
    else
        println("Cancelled.")
    end
end

"""
    run_lesson_with_mode(course_name, lesson, use_repl_mode)

Run a lesson in the appropriate mode.
"""
function run_lesson_with_mode(course_name, lesson, use_repl_mode)
    # Determine which mode to use
    use_repl = if use_repl_mode == :auto
        REPLMAKER_AVAILABLE[]
    elseif use_repl_mode == :repl
        if !REPLMAKER_AVAILABLE[]
            error("ReplMaker mode requested but ReplMaker.jl is not available. Install with: using Pkg; Pkg.add(\"ReplMaker\")")
        end
        true
    else  # :classic
        false
    end

    # Run the lesson
    if use_repl
        run_lesson_repl_mode(course_name, lesson)
        return true
    else
        if use_repl_mode == :auto && !REPLMAKER_AVAILABLE[]
            println("\n💡 Tip: Install ReplMaker.jl for syntax highlighting!")
            println("   Run: using Pkg; Pkg.add(\"ReplMaker\")\n")
        end
        run_lesson_classic_mode(course_name, lesson)
        return false
    end
end

function get_user_choice(prompt::String, range::AbstractRange)
    while true
        print(prompt)
        input = readline()
        try
            choice = parse(Int, input)
            if choice in range
                return choice
            else
                println("Please enter a number between $(first(range)) and $(last(range))")
            end
        catch
            println("Invalid input. Please enter a number.")
        end
    end
end

# Install the default course on first run
function install_default_course()
    create_basic_julia_course()
    println("✓ Installed 'Julia Basics' course\n")
end

end # module
