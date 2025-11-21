# Code parsing and evaluation

"""
    safe_eval(code_str)

Safely evaluate a string of Julia code and return the result.
"""
function safe_eval(code_str::AbstractString)
    code_str = String(code_str)
    try
        # Check if there are multiple lines/statements
        if occursin('\n', code_str) || occursin(';', code_str)
            # Parse all statements together
            block_str = "begin\n$(code_str)\nend"
            expr = Meta.parseall(block_str)  # ← Changed from Meta.parse to Meta.parseall
        else
            # Single expression
            expr = Meta.parse(code_str)
        end

        # Evaluate in Main module so variables persist
        result = Core.eval(Main, expr)

        return (success=true, result=result, error=nothing)
    catch e
        return (success=false, result=nothing, error=e)
    end
end

#=
"""
    check_answer(user_answer, expected_answer, question_type)

Check if the user's answer matches the expected answer based on question type.
"""
function check_answer(user_answer::AbstractString, expected_answer, question_type::Symbol)
    user_answer = String(user_answer)
    if question_type == :exact
        return strip(user_answer) == strip(string(expected_answer))
    elseif question_type == :multiple_choice
        try
            choice = parse(Int, strip(user_answer))
            return choice == expected_answer
        catch
            return false
        end
    elseif question_type == :code
        eval_result = safe_eval(user_answer)

        if !eval_result.success
            return (correct=false, message="Error: $(eval_result.error)")
        end

        # Check if result matches expected
        if eval_result.result == expected_answer
            return (correct=true, message="")
        elseif typeof(eval_result.result) == typeof(expected_answer)
            # Right type but wrong value
            return (correct=false, message="Not quite. You got $(eval_result.result), but the expected answer is $(expected_answer)")
        else
            return (correct=false, message="Your code produced $(eval_result.result) (type: $(typeof(eval_result.result)))")
        end
    end

    return false
end
=#
"""
    execute_code_silently(code)

Execute code without returning or displaying the result. Used for setup code.
"""
function execute_code_silently(code::AbstractString)
    code = String(code)
    try
        Core.eval(Main, Meta.parse(code))
        return true
    catch e
        println("Warning: Setup code failed: $e")
        return false
    end
end
