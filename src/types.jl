# Core data structures for Swirl


#=
"""
A question in a Swirl lesson. Can be multiple choice or require code evaluation.

Question types:
- :message - Display only, no answer required
- :code - Single expression or semicolon-separated statements
- :multistep_code - Multiple prompts, each building on previous code
- :multiple_choice - Select from choices
- :exact - Exact string match
"""
mutable struct Question
    text::MDLike
    type::Symbol  # :message, :multiple_choice, :code, :exact, :multistep_code
    answer::Any
    hint::MDLike
    choices::Vector{MDLike}  # For multiple choice
    validator::Union{Function,Nothing}  # Custom validation function
    steps::Vector{MDLike}  # For :multistep_code - text prompts for each step
    step_hints::Vector{MDLike}  # Hints for each step
    required_steps::Int  # Number of steps required
    setup::String  # Code to run before the question to set up variables of a previous julia session
end

Question(text, type, answer, hint="") = Question(text, type, answer, hint, String[], nothing)

# Multistep convenience ctor — accepts Vector{String} and delegates to the 10-arg ctor
function Question(text::MDLike,
    ::Val{:multistep_code},
    answer,
    hint::MDLike,
    steps::AbstractVector{<:MDLike},
    step_hints::AbstractVector{<:MDLike}=MDLike[],
    setup::String="")
    steps_v = MDLike[steps...]
    step_hints_v = isempty(step_hints) ? MDLike[fill("", length(steps_v))...] : MDLike[step_hints...]
    return Question(text, :multistep_code, answer, hint,
        String[], nothing,
        steps_v, step_hints_v, length(steps_v), setup)
end

Question(text, type, answer, hint="", setup="") = Question(text, type, answer, hint, MDLike[], nothing, MDLike[], MDLike[], 0, setup)

# Constructor for multistep questions
function Question(text::MDLike, ::Val{:multistep_code}, answer, hint::MDLike, steps::Vector{MDLike}, step_hints::Vector{MDLike}=MDLike[], setup::String="")
    if isempty(step_hints)
        step_hints = fill("", length(steps))
    end
    Question(text, :multistep_code, answer, hint, MDLike[], nothing, steps, step_hints, length(steps), setup)
end
=#
"""
A lesson containing a sequence of questions.
"""
struct Lesson{VQ}
    name::String             # identity (progress keys, directory names, menus)
    title::MDLike            # displayed title (can be Markdown)
    description::MDLike
    questions::VQ #Vector{Question}
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
    multistep_state::Dict{Int,Int}  # Maps question index to current step
end

LessonProgress(course::String, lesson::String) =
    LessonProgress(course, lesson, 1, false, 0, 0, Dict{Int,Int}())
