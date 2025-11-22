## --- Code Validators
## Validators expext
## * user_input -- text of user input
## * question -- question, maybe consult `answer` field
## * eval_result -- succesful result of safe_eval(user_input)
##
## should return a named tuple (correct::Bool, message::String)
abstract type CodeValidator end

#'  R ones
## We can test the:
## input: after parsing does expression match
## output: value is equal to a value
## output: value has the right type
##
#' * `calculates_same_value`: Test that the user's expression evaluates to a certain value.
#
# --> SameValueCodeValidator (has more defaults)
# --> same_value_validator
#'
#' * `any_of_exprs`: Test that the user's expression matches any of several possible expressions.
#
# --> ExpressionCodeValidator
# --> same_expression_validator

#'
#' * `expr_creates_var`: Test that a new variable has been created.
#'
# --> CreatesVarCodeValidator
#
#' * `expr_identical_to`: Test that the user has entered a particular expression.
#
# --> ExpressionCodeValidator
#'
#' * `expr_is_a`: Test that the expression itself is of a specific `class`.
#'
# --> IsaTypeCodeValidator
#
#' * `expr_uses_func`: Test that a particular function has been used.
#'
# --> UsesFunctionCodeValidator
#
#' * `func_of_newvar_equals`: Test the result of a computation such as \code{mean(newVar)} applied to a specific (user-named) variable created in a previous question.
#'
#' * `omnitest`: Test for a correct expression, a correct value, or both.
#'
#' * `val_has_length`: Test that the value of the expression has a particular `length`.
#'
# --> PredicateCodeValidator(x -> length(x) == 5, "Wrong length")
# --> OutputCodeValidator((x, qa) -> length(x) == 5, "Wrong length")
#
#' * `val_matches`: Test that the user's expression matches a regular expression (`regex`).
# --> PredicateCodeValidator(x -> !isnothing(match(r"regex", x)), "Doesn't match")
# --> OutputCodeValidator((x,_...) -> !isnothing(match(r"regex", x)), "Doesn't match")
#'
#' * `var_is_a`: Test that the \emph{value} of the expression is of a specific `class`.
#'
# --> IsaTypeCodeValidator
# --> same_type_validator
#'

# Validators are called after safe_eval so may be passed
# input, question, answer
# they return (; correct, message)
## checks input
struct InputValidator <: CodeValidator
    f        # Take f(input, q.answer)  return Boolean
    message
end
InputValidator(f) = InputValidator(f, "Input is not correct")

(v::InputValidator)(input, question, result) = begin

    correct = v.f(input, question.answer)
    message = correct ? "" : v.message

    (;correct, message)
end

# does expression have f as a call
_matched(ex::Symbol, f::Symbol) = ex == f
function _matched(ex, f::Symbol)
    hasproperty(ex, :head) || return false
    return any(Base.Fix2(_matched, f), ex.args)
end

# These functions return validators.
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

function call_function_validator(;
                                  message = "Expression does not contain the expected function call")
    λ = (input, question_answer) -> begin
        f = Symbol(question_answer)
        expr = Meta.parse(input)
        _matched(expr, f)
    end
    InputValidator(λ, message)
end

## --- validator that checks output (result)

struct OutputValidator <: CodeValidator
    f        # Take f(result, q.answer)  return Boolean
    message
end
OutputValidator(f) = OutputValidator(f, "Output is incorrect")

(v::OutputValidator)(input, question, result) = begin

    correct = v.f(result.result, question.answer)
    message = correct ? "" : v.message

    (;correct, message)
end

# instances
function same_value_validator(;
                              cmp=isequal,
                              message="Value does not match answer")
    f = (val, question_answer) -> begin
        cmp(val, question_answer)
    end
    OutputValidator(f, message)

end

function same_type_validator(; message ="Wrong type")

    f = (result, question_answer) -> begin
        correct_type = question_answer
        correct_types = applicable(iterate, correct_type) ? correct_type : (correct_type,)
        any(Base.Fix1(isa, result), correct_types)
    end
    OutputValidator(f, message)
end








## Or these



"""
    DefaultCodeValidator

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

# compare parsed input to expression(s)
"""
    ExpressionCodeValidator(exprs)

Compare parsed expression entered to an answer expression(s)

# Example
```
CodeQ(text = () -> md"Assign `2*x` to `y`",
      answer = nothing,
      hint = md"Just **type** `y = 2*x`",
      validator = ExpressionCodeValidator(Meta.parse("y=2x"))
      )
```

```
CodeQ(text = () -> md"Assign either `3*x` or `2*x` to `y`",
      answer = nothing,
      hint = md"Just **type** `y = 2*x`",
      validator = ExpressionCodeValidator((Meta.parse("y=2x"),Meta.parse("y=3x")))
      )
```

"""
struct ExpressionCodeValidator <: CodeValidator
    answer_expr
end

(v::ExpressionCodeValidator)(user_answer, question::CodeQuestion, eval_result) = begin

    input = Meta.parse(user_answer)
    target = v.answer_expr
    targets = applicable(iterate, target) ? target : (target,)

    correct =  any(isequal(input), targets)
    message = correct ? "" : "Expression does not match the expected expression"

    (; correct, message)
end

"""

Check typeof output
"""
struct IsaTypeCodeValidator <: CodeValidator
    answer_type
end
IsaTypeCodeValidator() = IsaTypeCodeValidator(nothing) # default to q.answer
(v::IsaTypeCodeValidator)(user_answer, question::CodeQuestion, eval_result) = begin

    correct_type = isnothing(v.answer_type) ? question.answer : v.answer_type
    correct_types = applicable(iterate, correct_type) ? correct_type : (correct_type,)

    correct = any(Base.Fix1(isa, eval_result.result), correct_types)
    message = correct ? "" : "Wrong type"

    return (; correct, message)
end

"""

Check that input uses a function
"""
struct UsesFunctionCodeValidator <: CodeValidator
    func::Symbol
    UsesFunctionCodeValidator(f) = new(Symbol(f))
end

(v::UsesFunctionCodeValidator)(user_answer, question::CodeQuestion, eval_result) = begin

    expr = Meta.parse(user_answer)
    f = v.func

    correct = _matched(expr, f)
    message = correct ? "" : "Expression does not call $f"
    return (; correct, message)
end


"""

Check that output matches a predicate
"""
struct PredicateCodeValidator{F,S} <: CodeValidator
    f::F
    message::S
end
PredicateCodeValidator(f) = PredicateCodeValidator(f, "Result is not a match")

(v::PredicateCodeValidator)(user_answer, question::CodeQuestion, eval_result) = begin

    correct = v.f(eval_result.result)
    message = correct ? "" : v.message
    return (; correct, message)
end

struct CreatesVarCodeValidator <: CodeValidator
    var::Symbol
end

(v::CreatesVarCodeValidator)(user_answer, question::CodeQuestion, eval_result) = begin
    correct = v.var ∈ names(Main)
    message = correct ? "" : "Variable $(v.var) was not created"
    (; correct, message)
end


## More code validators go here.
