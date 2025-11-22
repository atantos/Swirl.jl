## --- Code Validators
## Validators expext
## * user_input -- text of user input
## * question -- question, maybe consult `answer` field
## * eval_result -- succesful result of safe_eval(user_input)
##
## should return a named tuple (correct::Bool, message::String)
abstract type CodeValidator end
#'

# Validators are called after safe_eval so may be passed
# input, question, answer
# they return (; correct, message)

# Should export or make easy to import
# using Swirl: InputValidator, OutputValidator
# using Swirl:
#    same_value_validator,
#    same_expression_validator,
#    same_type_validator,
#    call_function_validator,
#    creates_var_validator


"""
    DefaultCodeValidator <: CodeValidator

Compare value of command to specific expected value

* [`answer`] -- if given uses this, otherwise the answer specified to the qeustion
* [`cmp`] -- by default, this uses `isequal` but any binary operation can be passed here
"""
struct DefaultCodeValidator <: CodeValidator
    answer_value # defaults to question. answer
    cmp          # defaults to isequal
end
DefaultCodeValidator(;answer = nothing, cmp = isequal) = DefaultCodeValidator(answer, cmp)

(v::DefaultCodeValidator)(user_answer, question::CodeQuestion, eval_result) = begin

    user_answer = String(user_answer)

    # Check if result matches expected
    expected_answer = isnothing(question.answer) ? v.answer_value : question.answer

    if v.cmp(eval_result.result, expected_answer)

        correct = true
        message = ""

    elseif typeof(eval_result.result) == typeof(expected_answer)

        correct = false
        message="Not quite. You got $(eval_result.result) the right type of answer, but not the expected answer."

    else

        correct=false
        message="Your code produced $(eval_result.result) (type: $(typeof(eval_result.result)))"

    end

    return (; correct, message)
end

# InputValidator and OutputValidator focus on either (input, question.answer)
# or (result.result, question.answer)

## --- validators that check the input

"""
    InputValidator(f, [message])

Validator to check input string

* `f`: function of `input` and `question_answer` returning a Boolean
* `message`: message to write if answers are all incorrect

# Example
```
        # match code by regular expression
        CodeQ(text = md"Enter any code that contains exp",
              answer = "Just any expression with `exp` would work",
              hint = "Write an expression",
              validator = InputValidator((input, question_answer) -> begin
                                         m =  match(r"exp", input)
                                         !isnothing(m)
                                         end)),
```
"""
struct InputValidator{F,S} <: CodeValidator
    f::F
    message::S
end
InputValidator(f) = InputValidator(f, "Input is not correct")

(v::InputValidator)(input, question, result) = begin

    correct = v.f(input, question.answer)
    message = correct ? "" : v.message

    (;correct, message)
end

# These functions return validators.

## check if user expression is a match

# this assumes answer is a string to be parsed by Meta.parse
# or a container of strings, each to be parsed
function same_expression_validator(;
                                   message = "Expression does not match the expected expression")

    f = (user_answer, question_answer) -> begin
        input = Meta.parse(user_answer)
        target =  isa(question_answer, AbstractString) ? Meta.parse(question_answer) :
            Meta.parse.(question_answer)
        targets = applicable(iterate, target) ? target : (target,)
        correct =  any(isequal(input), targets)
    end

    InputValidator(f, message)
end



# does expression have f as a call? Used to test if
# function is called
_matched(ex::Symbol, f::Symbol) = ex == f
function _matched(ex, f::Symbol)
    hasproperty(ex, :head) || return false
    return any(Base.Fix2(_matched, f), ex.args)
end

## check if user expression calls a function
function call_function_validator(;
                                  message = "Expression does not contain the expected function call")
    λ = (input, question_answer) -> begin
        f = Symbol(question_answer)
        expr = Meta.parse(input)
        _matched(expr, f)
    end
    InputValidator(λ, message)
end

## --- validators that check output (result)
"""
    OutputValidator(f, [message])

Validator to check output result

* `f`: function of `result` and `question_answer` returning a Boolean
* `message`: message to write if answers are all incorrect

The input has been evaluated successfully and `result` is its output.

# Example
```
# check that length of result is some amount
CodeQ(text = md"Enter a container with 4 elements",
      answer = "Many answers we possible, for example [1,2,3,4]",
      hint = "Pick some container type and fill it with 4 things",
      validator = OutputValidator((output, question_answer) -> begin
                                  length(output) == 4
                                  end))
```
"""
struct OutputValidator{F,S} <: CodeValidator
    f::F
    message::S
end
OutputValidator(f) = OutputValidator(f, "Output is incorrect")

(v::OutputValidator)(input, question, result) = begin

    correct = v.f(result.result, question.answer)
    message = correct ? "" : v.message

    (;correct, message)
end


## check if the output value matches
function same_value_validator(;
                              cmp=isequal,
                              message="Value does not match answer")
    f = (val, question_answer) -> begin
        cmp(val, question_answer)
    end
    OutputValidator(f, message)

end

## check if output type matches
function same_type_validator(; message ="Wrong type")

    f = (result, question_answer) -> begin
        correct_type = question_answer
        correct_types = applicable(iterate, correct_type) ? correct_type : (correct_type,)
        any(Base.Fix1(isa, result), correct_types)
    end
    OutputValidator(f, message)
end

## check if command created variable (in Main)
function creates_var_validator(; message = "Variable was not defined")
    f = (result, question_answer) -> begin
        var = Symbol(question_answer)
        var ∈ names(Main)
    end
    OutputValidator(f, message)
end

## More code validators go here.



# >>>>> DELETE THIS LATER <<<<<<
#'  R ones (https://github.com/swirldev/swirl/blob/master/R/answerTests2.R)
#'
#' * `calculates_same_value`: Test that the user's expression evaluates to a certain value.
#
# --> same_value_validator
#'
#' * `any_of_exprs`: Test that the user's expression matches any of several possible expressions.
#
# --> same_expression_validator

#'
#' * `expr_creates_var`: Test that a new variable has been created.
#'
# --> creates_var_validator
#
#' * `expr_identical_to`: Test that the user has entered a particular expression.
#
# --> same_expression_validator
#'
#' * `expr_is_a`: Test that the expression itself is of a specific `class`.
#'
# --> same_type_validator
#
#' * `expr_uses_func`: Test that a particular function has been used.
#'
# --> call_function_validator
#
#' * `func_of_newvar_equals`: Test the result of a computation such as \code{mean(newVar)} applied to a specific (user-named) variable created in a previous question.
#'
#' * `omnitest`: Test for a correct expression, a correct value, or both.
#'
#' * `val_has_length`: Test that the value of the expression has a particular `length`.
#'
# --> OutputValidator((x, qa) -> length(x) == 5, "Wrong length")
#
#' * `val_matches`: Test that the user's expression matches a regular expression (`regex`).
# --> OutputValidator((x,_...) -> !isnothing(match(r"regex", x)), "Doesn't match")
#'
#' * `var_is_a`: Test that the \emph{value} of the expression is of a specific `class`.
#'
