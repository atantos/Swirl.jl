# Base type for questions
abstract type AbstractQuestion end

## type for display only -- doesn't wait for prompt
"""
    OutputOnly <: AbstractQuestion

Abstract type for questions that only display information (no answer required).
These questions are not counted in scoring.
"""
abstract type OutputOnly <: AbstractQuestion end

## type for display with confirmation---but not a question
abstract type HasPrompt <: AbstractQuestion end
abstract type InputOutputOnly <: HasPrompt end

## type for Questions
abstract type QuestionType <: HasPrompt end

## AbstractQuestion base methods
"""
    show_question(question::AbstractQuestion)

Display a question's text and any question-specific formatting.
Uses multiple dispatch to customize display for different question types.
"""
function show_question(question::AbstractQuestion)
    txt = isa(question.text, Base.Callable) ? question.text() : question.text
    _show(txt)
    _show_question(question)
end

# Default: no additional display after text
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

# OutputOnly questions always return true (no wrong answer)
check_answer(input, ::OutputOnly) = true

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

"""
    MessageQ <: OutputOnly
    
Display a message to the user with no answer required.
"""
struct MessageQ <: OutputOnly
    text
end

# Constructor with keyword argument (not necessary but for consistency  with others)
MessageQ(; text="") = MessageQ(text)

## Include a file, e.g. one to generate a plot
## (how to include file with relative path?)
struct FileIncludeQ <: OutputOnly
    text
    file
end
FileIncludeQ(; text="", file="") = FileIncludeQ(text, file)

function _show_question(q::FileIncludeQ)
    f = expanduser(q.file)
    isfile(f) && Main.include(expanduser(q.file))
    println("")
end

## -- QuestionType
## Questions where answer is given at prompt; these are counted

## Type for Code questions
"""
    CodeQuestion <: AbstractQuestion

Abstract base type for questions that require code execution.
"""
abstract type CodeQuestion <: QuestionType end

"""
Default validator for code questions.
Executes the code and compares the result to the expected answer.
"""
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
"""
    CodeQ <: CodeQuestion

Single-step code question. User enters one expression (or semicolon-separated expressions)
and the result is checked against the expected answer.

# Fields
- `text`: Question text (String or Markdown)
- `answer`: Expected result value
- `hint`: Hint text to display if user asks for help
- `validator`: Optional custom validation function(input, answer) -> Bool
- `setup`: Optional code to run before question (e.g., to restore variables)

# Example
```julia
CodeQ(
    text = "Calculate the square root of 16",
    answer = 4.0,
    hint = "Use the sqrt() function"
)
```
"""
struct CodeQ <: CodeQuestion
    text
    answer
    answer_test # swirl.R makes use of this
    hint
    validator
    setup
end

CodeQ(; text="", answer="", answer_test="", hint="", validator=nothing, setup="") =
    CodeQ(text, answer, answer_test, hint, validator, setup)

"""
    MultistepCodeQ <: CodeQuestion

Multi-step code question. User enters code across multiple prompts (like a REPL session).
Each step can have its own prompt and hint.

# Fields
- `text`: Initial question text
- `answer`: Expected final result (or nothing if only execution matters)
- `hint`: General hint for the overall question
- `steps`: Vector of step-specific prompts
- `step_hints`: Vector of hints for each step
- `required_steps`: Number of steps that must be completed
- `validator`: Optional custom validation function
- `setup`: Optional setup code

# Example
```julia
MultistepCodeQ(
    text = "Create a function to calculate factorial",
    answer = 120,  # factorial(5)
    hint = "Use recursion or a loop",
    steps = [
        "Define the function signature",
        "Implement the base case", 
        "Implement the recursive case",
        "Test with factorial(5)"
    ],
    step_hints = [
        "factorial(n) = ...",
        "When n == 0 or n == 1, return 1",
        "Otherwise return n * factorial(n-1)",
        "Just call: factorial(5)"
    ]
)
```
"""
struct MultistepCodeQ <: CodeQuestion
    text
    answer
    # answer_test
    hint
    steps
    step_hints
    required_steps
    validator
    setup
end

# Constructor with keyword arguments
MultistepCodeQ(; text="", answer="", hint="", steps=[], step_hints=[],
    required_steps=nothing, validator=nothing, setup="") =
    MultistepCodeQ(text, answer, hint, steps, step_hints,
        isnothing(required_steps) ? length(steps) : required_steps,
        validator, setup)

# Convenience constructor from vectors
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

# Specialized version for multistep - takes both question AND state needed for handle_hint()
function _show_hint(q::MultistepCodeQ, state)
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
    StringQ(text, answer, hint, [validator]) <: AbstractQuestion

Match user answer as string. Supports exact matching, regex matching, or custom validators.

# Fields
* `text::{StringLike, Callable}`: text to display before question prompt; callable values are called with no arguments prior.
* `answer::{AbstractString, Regex, Callable}`: The default validtor depends on the type specified for `answer`. For strings an exact match is used, for Regular expressions `match` is used, otherwise `answer` is assumed to be a callable and the user answer is passed to it.
* `hint`: Hint text
* `validator`: Optional custom validator (overrides answer-based matching)
* `setup`: Optional setup code

# Examples
```julia
# Exact match
StringQ(text="What is the capital of France?", answer="Paris")

# Regex match  
StringQ(text="Name a programming language", answer=r"Julia|Python|Rust")

# Custom validator
StringQ(text="Enter a greeting", answer = s -> startswith(lowercase(s), "hello"))
```
"""
struct StringQ <: QuestionType
    text
    answer # string, regexp, function
    hint
    validator
    setup
end
StringQ(; text="", answer="", hint="", validator=nothing, setup="") =
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
    NumericQ(text, answer, hint, [validator]) <: AbstractQuestion

Compare answer numerically

* `answer::{Number, Tuple, Container}:` The default validation depends on the type of the sepcified `answer`. If answer is a number, an exact match on the user answer is made; if answer is a tuple, it is assumed to specify an interval, `(a,b)`, for which `a ≤ user_answer ≤ b` return true; otherwise, the test is `user_answer ∈ answer`, that is `answer` is a container of possible correct answers.
"""
struct NumericQ <: AbstractQuestion
    text
    answer # number, tuple--interval, container
    hint
    validator
    setup
end
NumericQ(; text="", answer=Inf, hint="", validator=nothing, setup="") =
    NumericQ(text, answer, hint, validator, setup)

# default is user_answer == answer
function check_answer(user_answer, question::NumericQ)

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

"""
    ChoiceQ <: ChoiceQuestion

Single-choice question. User selects one option by entering its number.
Replaces the old :multiple_choice type (which was actually single choice).

# Example
```julia
ChoiceQ(
    text = "Which is the largest planet?",
    choices = ["Earth", "Mars", "Jupiter", "Saturn"],
    answer = 3,  # Jupiter
    hint = "Think about gas giants"
)
```
"""
struct ChoiceQ <: ChoiceQuestion
    text
    choices  # Vector{String}
    answer::Int  # Index of correct choice
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
    MultipleChoiceQ(text, choices, answer::Vector{Int}, hint, [validator]) <: ChoiceQuestion

Multiple-choice question where user can select multiple options.
User enters comma-separated numbers (e.g., "1,3,4").

Default compares users choices (as a comma separated set of integers) to the answer specified as a vector of integers (after sorting).

# Example
```julia
MultipleChoiceQ(
    text = "Which are prime numbers?",
    choices = ["4", "5", "6", "7", "8", "9"],
    answer = [2, 4],  # 5 and 7
    hint = "Numbers only divisible by 1 and themselves"
)
```
"""
struct MultipleChoiceQ <: ChoiceQuestion
    text
    choices  # Vector{String}
    answer   # Vector{Int} - indices of correct choices
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
        println("Please enter comma-separated numbers for your selections (e.g., '1,3,4')")
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
LinkQ(; text="", link="") = LinkQ(text, link)

isaquestion(::LinkQ) = false

function _show_question(::LinkQ)
    println("")
    println("Open link? (yes/no)", "")
end

function _check_answer(user_answer::AbstractString, question::LinkQ)
    if !startswith(lowercase(user_answer), "n")
        run(`open $(question.link)`)
    end
    return true
end



# ============================================================================
# Backward Compatibility - Old Question() Constructor Style
# ============================================================================

"""
    Question

The actual Question() constructors return appropriate typed questions.
"""
struct Question
end

"""
    Question(text, type::Symbol, answer, hint="", setup="")

Backward-compatible constructor that returns appropriate question type.
Converts old `:type` symbol style to new dispatch-based types.

# Supported types
- `:message` -> MessageQ
- `:code` -> CodeQ  
- `:exact` -> StringQ or NumberQ (depending on answer type)

# Examples
```julia
# These old-style calls:
Question("Hello!", :message, nothing)
Question("Calculate 2+2", :code, 4, "Add them")
Question("What is the capital?", :exact, "Paris", "Think France")

# Are equivalent to:
MessageQ("Hello!")
CodeQ(text="Calculate 2+2", answer=4, hint="Add them")
StringQ(text="What is the capital?", answer="Paris", hint="Think France")
```
"""
function Question(text, type::Symbol, answer, hint="", setup="")
    validator = nothing
    if type == :message
        return MessageQ(text)
    elseif type == :code
        return CodeQ(text, answer, "", hint, validator, setup)
    elseif type == :exact
        if isa(answer, Number)
            return NumberQ(text, answer, hint, validator, setup)
        else
            return StringQ(text, answer, hint, validator, setup)
        end
    else
        error("Unknown question type: $type. Use :message, :code, or :exact (or use typed constructors directly)")
    end
end

"""
    Question(text, type::Symbol, answer, hint, choices::Vector, validator=nothing, setup="")

Backward-compatible constructor for questions with choices.
"""
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

"""
    Question(text, Val{:multistep_code}, answer, hint, steps, step_hints=[], setup="")

Backward-compatible constructor for multistep code questions.
"""
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
