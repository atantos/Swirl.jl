## --- Code Validators

# >>>>> DELETE THIS LATER <<<<<<
#'  R ones (https://github.com/swirldev/swirl/blob/master/R/answerTests2.R)
#'  and their counterparts with these validators
#'
#' * `calculates_same_value`: Test that the user's expression evaluates to a certain value.
#
# --> same_value_validator
#'
#' * `any_of_exprs`: Test that the user's expression matches any of several possible expressions.
#
# --> reduce ∪ over collection of same_expression_validator
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
# --> has_expression_validator("sin(_)") or
#     has_expression_validator("sin") or
#
#' * `func_of_newvar_equals`: Test the result of a computation such as \code{mean(newVar)} applied to a specific (user-named) variable created in a previous question.
#'
#' * `omnitest`: Test for a correct expression, a correct value, or both.
#
#  ---> use ∪ to combine e.g
#       same_value_validator(a) ∪ same_expression_validator("a")
#'
#' * `val_has_length`: Test that the value of the expression has a particular `length`.
#'
# --> OutputValidator((x,_) -> length(x) == 5, "Wrong length")
#
#' * `val_matches`: Test that the user's expression matches a regular expression (`regex`).
# --> match_validator(r::Regex)
#'
#' * `var_is_a`: Test that the \emph{value} of the expression is of a specific `class`.
#'
# ---> same_type_validator(SomeType)
# <<<<<<<<<<<<<<<<<<<<
#
# Should export, or make easy to import, or have Validator module, or ...
# using Swirl:
# using Swirl:
#    InputValidator,                # Validate input
#    OutputValidator,               # validate successful output
#    same_expression_validator,     # input, as expression, is equal to expression
#    has_expression_validator,      # input, as expression, has subexpression
#    match_input_validator,         # match(re, input), as string
#    same_value_validator,          # output has correct value
#    in_interval_validator,         # a ≤ output ≤ b
#    in_range_validator,            # output ∈ answer
#    same_type_validator,           # output has correct type
#    match_output_validator         # match(re, output)
#    creates_var_validator,         # output leaves variable in Main
#    creates_function_validator     # output is function and evaluates on specified values

"""
    AbstractValidator

Abstract class for a validator.

A validator is a callable struct which expects as input `(input, question, result)` where `input` is the string the user entered; `question` the given question (most likely used to lookup `question.answer`); and `result` the result of `safe_eval(input)` (always when `success=true`).

A validator should return a named tuple with names `correct::Bool` indicating if the validator passed and `message` for display in the case the user never validates their input.

Validators may be combined using:

* the `∘` infix operator (typed with `\\circ[tab]`) if *all* of the validators need to be correct, for the combined one to be;

* the `∪` infix operator (typed with `\\cup[tab]`)  if *any* of validators need to be correct, for the combined one to be.

The main validator subtypes are `InputValidator` and `OutputValidator` to specialize on just the user input or its output value.
"""
abstract type AbstractValidator end

# Composed Validators must `all` be correct
# Validators can be composed with ∘ `\circ[tab]`
"""
    ComposedValidator

Struct to hold composed validators.

A validator comprised of a collection of validators. This validator is correct if **all** of the validators is correct. The infix union operator `\\circ[tab]` can be used to combine validators.


# Example
```
ispositivev = OutputValidator((output,_) -> output > 0, "Not positive")
isintegerv = same_type_validator(Integer)

CodeQ(text = () -> md"Enter a positive Integer",
      answer = "Any positive integer",
      hint   = "Any positive integer",
      validator = ispositivev ∘ isintegerv
      )
```
"""
struct ComposedValidator{T} <: AbstractValidator
    Vs::T
end

Base.:∘(v1::AbstractValidator, v2::AbstractValidator) = ComposedValidator((v1, v2))
Base.:∘(v1::AbstractValidator, v2::ComposedValidator) =
    ComposedValidator((v1, v2.Vs...))
Base.:∘(v1::ComposedValidator, v2::AbstractValidator) =
    ComposedValidator((v1.Vs..., v2))
Base.:∘(v1::ComposedValidator, v2::ComposedValidator) =
    ComposedValidator((v1.Vs..., v2.Vs...))

(v::ComposedValidator)(input, question, result) = begin
    for Vᵢ in reverse(v.Vs)
        correct, message = Vᵢ(input, question, result)
        !correct && return (correct, message)
    end
    (;correct=true, message="")
end

# OneOfMany requires `any` of the validators to be correct
# we can use infix ∪ `\cup[tab]` to combine validators
# example: either or expression:
# `reduce(∪,same_expression_validator.(("y=2x", "y=3x")))`
"""
    OneOfManyValidator

A validator comprised of a collection of validators. This validator is correct if **any** of the validators is correct. The infix union operator `\\cup[tab]` can be used to combine validators.

# Example
```
# validator which accepts either `y=2x` or `y=3x` for a correct answer
reduce(∪, same_expression_validator.(("y=2x", "y=3x")))
```
"""
struct OneOfManyValidator <: AbstractValidator
    Vs
end

Base.:∪(v1::AbstractValidator, v2::AbstractValidator) = OneOfManyValidator((v1, v2))
Base.:∪(v1::AbstractValidator, v2::OneOfManyValidator) =
    OneOfManyValidator((v1, v2.Vs...))
Base.:∪(v1::OneOfManyValidator, v2::AbstractValidator) =
    OneOfManyValidator((v1.Vs..., v2))
Base.:∪(v1::OneOfManyValidator, v2::OneOfManyValidator) =
    OneOfManyValidator((v1.Vs..., v2.Vs...))

# check if any are true
(v::OneOfManyValidator)(input, question, result) = begin
    message = ""
    for vᵢ in v.Vs
        (; correct, message) = vᵢ(input, question, result)
        correct && return (;correct, message)
    end
    correct = false
    return (;correct, message) # last message
end


# The default CodeQuestion validator
"""
    EqualValueValidator <: AbstractValidator

Compare value of command to specific expected value

* [`answer`] -- if given uses this, otherwise the `answer` specified to the question
* [`cmp`] -- by default, this uses `isequal` but any binary operation can be passed here

This is the default validator for `CodeQ` questions.
"""
struct EqualValueValidator <: AbstractValidator
    answer_value # defaults to question.answer
    cmp          # defaults to isequal
end
EqualValueValidator(;answer = nothing, cmp = isequal) = EqualValueValidator(answer, cmp)

(v::EqualValueValidator)(input, question, result) = begin

    # Check if result matches expected
    expected_answer = isnothing(question.answer) ? v.answer_value : question.answer

    if isa(expected_answer, Base.Callable)

        correct = expected_answer(result.result)
        message = "Try again"

    elseif v.cmp(result.result, expected_answer)

        correct = true
        message = ""

    elseif typeof(result.result) == typeof(expected_answer)

        correct = false
        message = "Not quite. You got $(result.result) the right type of answer, but not the expected answer."

    else

        correct = false
        message = "Your code produced $(result.result) (type: $(typeof(result.result)))"

    end

    return (; correct, message)
end

## --- InputValidator and OutputValidator focus on either (input, question.answer)
## or (result.result, question.answer)

## --- validators that check the input

"""
    InputValidator(f, [message])

Validator to check input string

* `f`: function of `input` and `question_answer` returning a Boolean
* `message`: message to write if answers are all incorrect

# Example
```
# match code by regular expression (see `match_input_validator`)
CodeQ(text = md"Enter any code that contains `exp`",
      answer = "Just any expression with `exp` would work",
      hint   = "Write an expression",
      validator = InputValidator((input, question_answer) -> begin
                                 m =  match(r"exp", input)
                                 !isnothing(m)
                                 end))
```
"""
struct InputValidator{F,S} <: AbstractValidator
    f::F
    message::S
end
InputValidator(f) = InputValidator(f, "Input is not correct")

(v::InputValidator)(input, question, result) = begin

    correct = v.f(input, question.answer)
    message = correct ? "" : v.message

    (;correct, message)
end

# util
# Does expression have f as subexpression?
# use (_) as wildcard
function _has(ex, f)
    f == :(_) && return true
    ex == f && return true
    hasproperty(ex, :head) || return false
    if ex.head == :call
        exf, exa... = ex.args
        isa(f, Symbol) && exf == f && return true
    end
    hasproperty(f,  :head) || return false
    (ex.head == f.head == :call) || return false


    ff,  fa... = f.args

    if exf == ff # head matches, do all arguments?
        for (i, exaᵢ) in enumerate(exa)
            fᵢ = fa[i]
            fᵢ == :(_) && return true
            _has(exaᵢ, fᵢ) || return false
        end
        length(exa) == length(fa) && return true
    end

    # else recurse and see if any match
    return any(_has(exaᵢ,f) for exaᵢ ∈ exa)

end

##--- These functions return validators.


## check if user expression is a match to answer using Meta.parse to normalize
# this assumes answer is a string to be parsed by Meta.parse
# or a container of strings, each to be parsed
function same_expression_validator(answer = nothing;
                                   message = "Expression does not match the expected expression")

    f = (user_answer, question_answer) -> begin
        a = isnothing(answer) ? question_answer : answer
        input = Meta.parse(user_answer)
        # answer can be given as a string or an expression
        target =  isa(a, AbstractString) ? Meta.parse(a) : a
        correct =  isequal(input, target)
    end

    InputValidator(f, message)
end

## Does the input expression contain the answer expression
# can  use _ as a wildcard for the answer
# This is a match in the expression tree---it isn't commutative or associative!
function has_expression_validator(answer=nothing;
                                  message = "Input expression does not contain the expected expression")

    f = (input, question_answer) -> begin
        a = isnothing(answer) ? question_answer : answer
        pat = isa(a, String) ? Meta.parse(a) : a
        expr = Meta.parse(input)
        _has(expr, pat)
    end

    InputValidator(f, message)

end

## Does input match given regular expression
function match_input_validator(answer=nothing;
                               message = "Expression does not contain the expression")

    f = (input, question_answer) -> begin
        pat = isnothing(answer) ? question_answer : answer
        if !isa(pat, Regex)
            @warn "pattern is not a regular expression"
            return false
        end
        m = match(pat, input)
        return !isnothing(m)
    end

    InputValidator(f, message)

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
struct OutputValidator{F,S} <: AbstractValidator
    f::F
    message::S
end
OutputValidator(f) = OutputValidator(f, "Output is incorrect")

(v::OutputValidator)(input, question, result) = begin

    correct = v.f(result.result, question.answer)
    message = correct ? "" : v.message

    (;correct, message)

end


## check if the output value matches via `cmp`, defaulting to `isequal`
## cmp(value, answer)
function same_value_validator(answer=nothing;
                              cmp=isequal,
                              message="Value does not match answer")
    f = (val, question_answer) -> begin
        a = isnothing(answer) ? question_answer : answer
        cmp(val, a)
    end

    OutputValidator(f, message)

end

# is value in [l,r] interval: l ≤ value ≤ r
function in_interval_validator(answer=nothing;
                            message="Not in interval")
    f = (value, question_answer) -> begin
        a = isnothing(answer) ? question_answer : answer
        l, r= extrema(a)
        return l ≤ value ≤ r
    end

    OutputValidator(f, message)
end

# is value in container (value ∈ answer)
function in_range_validator(answer=nothing;
                            message="Not in range")
    f = (value, question_answer) -> begin
        a = isnothing(answer) ? question_answer : answer
        return value ∈ a
    end
    OutputValidator(f, message)
end


## check if output type matches specific type via `isa(result, type)`
## use a Union if more than one.
function same_type_validator(answer=nothing;
                             message ="Wrong type")

    f = (result, question_answer) -> begin
        correct_type = isnothing(answer) ? question_answer : answer
        isa(result, correct_type)
    end

    OutputValidator(f, message)

end

# does value match regular expression?
function match_output_validator(answer=nothing;
                               message = "Answer does not match")
    f = (result, question_answer) -> begin
        a = isnothing(answer) ? question_answer : answer
        re = !isa(a, Regex) ? Regex(a) : a
        m = match(re, result)
        return !isnothing(m)
    end

    OutputValidator(f, message)

end

## check if command created variable (in Main)
function creates_var_validator(answer=nothing;
                               message = "Variable was not defined")
    f = (result, question_answer) -> begin
        a = isnothing(answer) ? question_answer : answer
        var = Symbol(a)
        var ∈ names(Main)
    end

    OutputValidator(f, message)

end

# is the output a function *and*
# does it match each pair of input=>output values specified to the answer?
function creates_function_validator(answer=nothing;
                                    message = "Function did not evaluate correctly")
    f = (result, question_answer) -> begin

        isa(result, Base.Callable) || return false

        a = isnothing(answer) ? question_answer : answer
        as = isa(a, Pair) ? (a,) : a

        # a is a collection of pairs in=>out
        for aᵢ ∈ as
            i,o = aᵢ
            Base.invokelatest(result, i) == o || return false
        end

        return true
    end

    OutputValidator(f, message)

end

## More code validators go here.
