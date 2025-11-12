# Core data structures for Swirl

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
    text::String
    type::Symbol  # :message, :multiple_choice, :code, :exact, :multistep_code
    answer::Any
    hint::String
    choices::Vector{String}  # For multiple choice
    validator::Union{Function,Nothing}  # Custom validation function
    steps::Vector{String}  # For :multistep_code - text prompts for each step
    step_hints::Vector{String}  # Hints for each step
    required_steps::Int  # Number of steps required
    setup::String  # Code to run before the question to set up variables of a previous julia session
end

Question(text, type, answer, hint="", setup="") = Question(text, type, answer, hint, String[], nothing, String[], String[], 0, setup)

# Constructor for multistep questions
function Question(text::String, ::Val{:multistep_code}, answer, hint::String, steps::Vector{String}, step_hints::Vector{String}=String[], setup::String="")
    if isempty(step_hints)
        step_hints = fill("", length(steps))
    end
    Question(text, :multistep_code, answer, hint, String[], nothing, steps, step_hints, length(steps), setup)
end

"""
A lesson containing a sequence of questions.
"""
struct Lesson
    name::String
    description::String
    questions::Vector{Question}
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

