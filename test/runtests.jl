using Test
using Swirl

@testset "Swirl.jl Tests" begin

    @testset "Basic Types" begin
        # Test Question creation
        q = Swirl.Question("Test question", :code, 42, "Test hint")
        @test q.text == "Test question"
        @test q.type == :code
        @test q.answer == 42
        @test q.hint == "Test hint"

        # Test Lesson creation
        lesson = Swirl.Lesson("Test Lesson", "Description", [q])
        @test lesson.name == "Test Lesson"
        @test length(lesson.questions) == 1

        # Test Course creation
        course = Swirl.Course("Test Course", "Description", [lesson])
        @test course.name == "Test Course"
        @test length(course.lessons) == 1
    end

    @testset "Code Evaluation" begin
        # Test successful evaluation
        result = Swirl.safe_eval("2 + 2")
        @test result.success == true
        @test result.result == 4

        # Test error handling
        result = Swirl.safe_eval("undefined_binding")
        @test result.success == false
        @test result.error !== nothing
    end

    @testset "Answer Checking" begin
        # Test exact answer
        @test Swirl.check_answer("42", "42", :exact) == true
        @test Swirl.check_answer("42", "43", :exact) == false

        # Test multiple choice
        @test Swirl.check_answer("2", 2, :multiple_choice) == true
        @test Swirl.check_answer("1", 2, :multiple_choice) == false

        # Test code answer
        result = Swirl.check_answer("5 + 3", 8, :code)
        @test result.correct == true
    end

    @testset "Progress Tracking" begin
        # Test progress creation
        progress = Swirl.LessonProgress("Test Course", "Test Lesson")
        @test progress.current_question == 1
        @test progress.completed == false
        @test progress.correct_answers == 0
    end

    @testset "Course Management" begin
        # Test getting available courses
        courses = Swirl.get_available_courses()
        @test length(courses) >= 1
        @test courses[1].name == "Julia Basics"

        # Test basic Julia course structure
        basic_course = Swirl.create_basic_julia_course()
        @test length(basic_course.lessons) == 3
        @test basic_course.lessons[1].name == "Basic Math and Variables"
    end

end

include("test-validators.jl")
