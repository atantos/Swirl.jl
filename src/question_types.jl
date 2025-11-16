# Base type for questions
abstract type AbstractQuestion end

## type for display only -- doesn't wait for prompt
abstract type OutputOnly <: AbstractQuestion end

## type for display with confirmation---but not a question
abstract type HasPrompt <: AbstractQuestion end
abstract type InputOutputOnly <: HasPrompt end

## type for Questions
abstract type QuestionType <: HasPrompt end

## AbstractQuestion base methods
function show_question(question::AbstractQuestion)
    txt = isa(question.text, Base.Callable) ? question.text() : question.text
    _show(txt)
    _show_question(question)
end

_show_question(q::AbstractQuestion) = nothing

## HasPrompt base methods
## process prompt
function check_answer(input, question::HasPrompt)
    if hasproperty(question, :validator) && !isnothing(question.validator)
        question.validator(input, question) # not sure what to pass of question
    else
        _check_answer(input, question)
    end
end

## QuestionType base methods
function _show_hint(q::QuestionType)
    hint = isa(q.hint, Base.Callable) ? q.hint() : q.hint
    if !isempty(hint)
        print("💡 Hint: ")
        _show(hint)
    end
end

# Question type trait indicating if question type should
# be scored
isaquestion(::AbstractQuestion) = false
isaquestion(::QuestionType) = true

## -- OutputOnly
## Doesn't wait for a prompt

# for a message
struct MessageQ <: OutputOnly
    text
end
MessageQ(;text="") = MessageQ(text)

## Include a file, e.g. one to generate a plot
## (how to include file with relative path?)
struct FileIncludeQ <: OutputOnly
    text
    file
end
FileIncludeQ(;text="", file="") = FileIncludeQ(text, file)

function _show_question(q::FileIncludeQ)
    f = expanduser(q.file)
    isfile(f) &&  Main.include(expanduser(q.file))
    println("")
end

## -- QuestionType
## Questions where answer is given at prompt; these are counted

## Type for Code questions
abstract type CodeQuestion <: QuestionType end

# default validator for code
function _check_answer(user_answer::AbstractString, question::CodeQuestion)
    user_answer = String(user_answer)
    eval_result = safe_eval(user_answer)
    if !eval_result.success
        return (correct=false, message="Error: $(eval_result.error)")
    end

    # Check if result matches expected
    expected_answer = question.answer
    if eval_result.result == expected_answer
        return (correct=true, message="")
    elseif typeof(eval_result.result) == typeof(expected_answer)
        # Right type but wrong value
        return (correct=false, message="Not quite. You got $(eval_result.result), but the expected answer is $(expected_answer)")
    else
        return (correct=false, message="Your code produced $(eval_result.result) (type: $(typeof(eval_result.result)))")
    end

    return false
end

# Single step code question
struct CodeQ <: CodeQuestion
    text
    answer
    answer_test # swirl.R makes use of this
    hint
    validator
    setup
end

CodeQ(;text="", answer="", answer_test="", hint="", validator=nothing, setup="") =
    CodeQ(text, answer, answer_test, hint, validator, setup)

# multi-step code question
struct MultistepCodeQ <: CodeQuestion
    text
    answer
    answer_test
    hint
    steps
    step_hints
    required_steps
    validator
    setup
end

MultistepCodeQ(;text="", answer="", answer_test="", hint="", validator=nothing,setup="") =
    MultistepCodeQ(text, answer, answer_test, hint, validator, setup)

function MultistepCodeQ(text::MDLike,
    answer,
    hint::MDLike,
    steps::AbstractVector{<:MDLike},
    step_hints::AbstractVector{<:MDLike}=MDLike[],
    setup::String="")
    steps_v = MDLike[steps...]
    step_hints_v = isempty(step_hints) ? MDLike[fill("", length(steps_v))...] : MDLike[step_hints...]
    return MultistepCodeQ(
        text, answer, hint,
        steps_v, step_hints_v, length(steps_v),
        nothing, setup)
end

function _show_hint(q::MultistepCodeQ)
    # For multistep questions, show step-specific hint
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
end


"""
    StringQ(text, answer, hint, [validator])

Match an user answer as strings.

* `text::{StringLike, Callable}`: text to display before question prompt; callable values are called with no arguments prior.
* `answer::{AbstractString, Regex, Callable}`: The default validtor depends on the type specified for `answer`. For strings an exact match is used, for Regular expressions `match` is used, otherwise `answer` is assumed to be a callable and the user answer is passed to it.
"""
struct StringQ <: QuestionType
    text
    answer # string, regexp, function
    hint
    validator
    setup
end
StringQ(;text="", answer="", hint="", validator=nothing, setup="") =
    StringQ(text, answer, hint, validator, setup)

function check_answer(user_input, question::StringQ)

    eval_result = safe_eval(user_input)
    if !eval_result.success
        return (correct=false, message="Error: $(eval_result.error)")
    elseif !isa(eval_result.result, AbstractString)
        return (correct=false, message="Error: answer is not a string")
    end

    user_answer = eval_result.result
    answer = question.answer

    if isa(answer, AbstractString)
        return user_answer == answer
    elseif isa(answer, Regex)
        m = match(answer, user_answer)
        return isnothing(m) ? false : true
    else # assume callable
        return answer(user_answer)
    end
end


"""
    NumberQ(text, answer, hint, [validator])

Compare answer numerically

* `answer::{Number, Tuple, Container}:` The default validation depends on the type of the sepcified `answer`. If answer is a number, an exact match on the user answer is made; if answer is a tuple, it is assumed to specify an interval, `(a,b)`, for which `a ≤ user_answer ≤ b` return true; otherwise, the test is `user_answer ∈ answer`, that is `answer` is a container of possible correct answers.
"""
struct NumberQ <: QuestionType
    text
    answer # number, tuple--interval, container
    hint
    validator
    setup
end
NumberQ(;text="", answer=Inf, hint="", validator=nothing, setup="") =
    NumberQ(text, answer, hint, validator, setup)

# default is user_answer == answer
function check_answer(user_answer, question::NumberQ)

    eval_result = safe_eval(user_answer)
    if !eval_result.success
            return (correct=false, message="Error: $(eval_result.error)")
    elseif !isa(eval_result.result, Number)
        return (correct=false, message="Error: answer is not a number")
    end


    answer = question.answer
    if isa(answer, Number)
        return eval_result.result == answer
    elseif isa(answer, Tuple)
        a, b = extrema(answer)
        return a ≤ eval_result.result ≤ b
    else
        return eval_result.result ∈ answer
    end
end

"""
    ChoiceQuestion(type, choices, answer::Int, hint, [validator])

Compares user choice (as an integer) to answer.

* `choices::{Iterable}` iterable containing each choice specified as string or a callable that returns a string.

"""
abstract type ChoiceQuestion <: QuestionType end
function _show_question(question::ChoiceQuestion)
    println("")
    for (i, choice) in enumerate(question.choices)
        txt = isa(choice, Base.Callable) ? choice() : choice
        print("  $i. ")
        _show(txt)
    end
    println()
end

struct ChoiceQ <: ChoiceQuestion
    text
    choices  # Vector{String}
    answer::Int
    hint
    validator
    setup
end
ChoiceQ(; text="", choices=[], answer=0, hint="", validator=nothing, setup="") =
    ChoiceQ(text, choices, answer, hint, validator, setup)

function check_answer(user_input, question::ChoiceQ)
    try
        user_answer = parse(Int, user_input)
        return user_answer == question.answer
    catch
        println("Answer is the corresponding number for the item you wish to select")
        return false
    end
end

"""
    MultipleChoiceQ(text, choices, answer::Vector{Int}, hint, [validator])

Default compares users choices (as a comma separated set of integers) to the answer specified as a vector of integers (after sorting).
"""
struct MultipleChoiceQ <: ChoiceQuestion
    text
    choices  # Vector{String}
    answer   # Vector{Int}
    hint
    validator
    setup
end
MultipleChoiceQ(; text="", choices=String[], answer=Int[], hint="", validator=nothing, setup="") =
    MultipleChoiceQ(text, choices, answer, hint, validator, setup)

function check_answer(user_input, question::MultipleChoiceQ)
    try
        user_answer = parse.(Int, split(user_input, r",\s*|\s+"))
        return sort(user_answer) == sort(question.answer)
    catch
        println("Answer is the corresponding number(s) for the item(s) you wish to select")
        return false
    end
end

## --- InputOutputOnly
## has prompt, but not a question. For example, for confirmation

## open a link, such as a video, if user says yes
struct LinkQ <: InputOutputOnly
    text
    link
end
LinkQ(;text="", link="") = LinkQ(text, link)

isaquestion(::LinkQ) = false

function _show_question(::LinkQ)
    println("")
    println("Open link? (yes/no)","")
end

function _check_answer(user_answer::AbstractString, question::LinkQ)
    if !startswith(lowercase(user_answer), "n" )
        run(`open $(question.link)`)
    end
    return true
end



## ------

# code to handle swirl.R-type question style
struct Question
end

function Question(text, type, answer, hint="", setup="")
    validator = nothing
    if type == :message
        return MessageQ(text)
    elseif type == :code
        return CodeQ(text, answer, "", hint, validator, setup)
    elseif type == :exact
        return NumberQ(text, answer, hint, validator, setup)
    end
end

function Question(text, type::Symbol, answer, hint, choices::Vector,
                  validator=nothing, setup="")

    if type == :message
        return MessageQ(text)
    elseif type == :multiple_choice
        return ChoiceQ(text, choices, answer, hint, validator, setup)
    elseif type == :code
        return CodeQ(text, answer, "", hint, validator, setup)
    elseif type == :exact
        if isa(answer, Number)
            return NumericQ(text, answer, hint, validator, setup)
        else
            return StringQ(text, answer, hint, validator, setup)
        end
    else
        error("unknown type $type")
    end
end

function Question(text::MDLike, ::Val{:multistep_code}, answer, hint::MDLike,
                  steps::Vector, step_hints::Vector=MDLike[],
                  setup::String="")
    if isempty(step_hints)
        step_hints = fill("", length(steps))
    end
    MultistepCodeQ(text, answer, "", hint,
                   steps, step_hints, length(steps),
                   nothing, setup)
end
