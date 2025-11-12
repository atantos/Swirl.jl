# Core data structures for Swirl

#=
"""
A question in a Swirl lesson. Can be multiple choice or require code evaluation.
"""
mutable struct Question
    text::String
    type::Symbol  # :message, :multiple_choice, :code, :exact
    answer::Any
    hint::String
    choices::Vector{String}  # For multiple choice
    validator::Union{Function,Nothing}  # Custom validation function
end

Question(text, type, answer, hint="") = Question(text, type, answer, hint, String[], nothing)
=#

# XXX stubs to handle default questions a narrowing of a run_question_classic to Question
struct Question
end
function Question(text, type, answer, hint="")
    if type == :message
        return MessageQ(text)
    elseif type == :code
        return CodeQ(text, answer, hint, nothing)
    elseif type == :exact
        return NumberQ(text, answer, hint, nothing)
    end
end

function Question(text, type, answer, hint, choices, validator=nothing)
    if type == :message
        return MessageQ(text)
    elseif type == :multiple_choice
        return ChoiceQ(text, choices, answer, hint, validator)
    elseif type == :code
        return CodeQ(text, answer, hint, validator)
    elseif type == :exact
        if isa(answer, Number)
            return NumericQ(text, answer, hint, validator)
        else
            return StringQ(text, answer, hint, validator)
        end
    else
        error("unknown type")
    end
end
## XXX end stubs

## -----
abstract type AbstractQuestion end
function show_question(question::AbstractQuestion)
    txt = isa(question.text, Base.Callable) ? question.text() : question.text
    isa(txt, AbstractString) ? println(txt) : display(txt)

    _show_question(question)
end
_show_question(q::AbstractQuestion) = nothing
function check_answer(input, question::AbstractQuestion)
    if hasproperty(question, :validator) && !isnothing(question.validator)
        question.validator(input, question)
    else
        _check_answer(input, question)
    end
end

abstract type OutputOnly <: AbstractQuestion end
struct MessageQ <: OutputOnly
    text
end
MessageQ(;text="") = MessageQ(text)

# improvements:
# * a better comparison?
# * the REPL doesn't allow multiple-line input
struct CodeQ <: AbstractQuestion
    text
    answer
    hint
    validator
end
CodeQ(;text="", answer="", hint="", validator=nothing) =
    CodeQ(text, answer, hint, validator)

# default for code
function _check_answer(user_answer::AbstractString, question::CodeQ)
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

"""
    StringQ(text, answer, hint, [validator])

Match an user answer as strings.

* `answer::{AbstractString, Regex, Callable}`: The default validtor depends on the type specified for `answer`. For strings an exact match is used, for Regular expressions `match` is used, otherwise `answer` is assumed to be a callable and the user answer is passed to it.
"""
struct StringQ <: AbstractQuestion
    text
    answer # string, regexp, function
    hint
    validator
end
StringQ(;text="", answer="", hint="", validator=nothing) =
    StringQ(text, answer, hint, validator)

function check_answer(user_answer, question::StringQ)
    user_answer = String(user_answer)
    answer = question.answer

    if isa(answer, AbstractString)
        # Strings are bit off with this REPL experience
        return user_answer == "\"$answer\"" || user_answer == answer
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
struct NumberQ <: AbstractQuestion
    text
    answer # number, tuple--interval, container
    hint
    validator
end
NumberQ(;text="", answer=Inf, hint="", validator=nothing) =
    NumberQ(text, answer, hint, validator)

# default is user_answer == answer
function check_answer(user_answer, question::NumberQ)
    eval_result = safe_eval(user_answer)
    if !eval_result.success
            return (correct=false, message="Error: $(eval_result.error)")
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
"""
abstract type ChoiceQuestion <: AbstractQuestion end
function _show_question(question::ChoiceQuestion)
    for (i, choice) in enumerate(question.choices)
        txt = isa(choice, Base.Callable) ? choice() : choice
        print("  $i.")
        isa(txt, AbstractString) ? println(" ", txt) : display(txt)
    end
    println()
end

struct ChoiceQ <: ChoiceQuestion
    text
    choices  # Vector{String}
    answer::Int
    hint
    validator
end
ChoiceQ(; text="", choices=[], answer=0, hint="", validator=nothing) =
    ChoiceQ(text, choices, answer, hint, validator)

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
end
MultipleChoiceQ(; text="", choices=String[], answer=Int[], hint="", validator=nothing) =
    MultipleChoiceQ(text, choices, answer, hint, validator)

function check_answer(user_input, question::MultipleChoiceQ)
    try
        user_answer = parse.(Int, split(user_input,","))
        return sort(user_answer) == sort(question.answer)
    catch
        println("Answer is the corresponding number(s) for the item(s) you wish to select")
        return false
    end
end


"""
A lesson containing a sequence of questions.
"""
struct Lesson
    name::String
    description::String
    questions#::Vector{Question}
end

"""
A course containing multiple lessons.
"""
struct Course
    name::String
    description::String
    lessons::Vector{Lesson}
end

"""
Progress tracking for a specific lesson.
"""
mutable struct LessonProgress
    course_name::String
    lesson_name::String
    current_question::Int
    completed::Bool
    correct_answers::Int
    attempts::Int
end

LessonProgress(course::String, lesson::String) =
    LessonProgress(course, lesson, 1, false, 0, 0)
