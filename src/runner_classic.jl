

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
