using Swirl: InputValidator, OutputValidator, EqualValueValidator
using Swirl:
    same_expression_validator,
    has_expression_validator,
    match_input_validator, # input
    match_output_validator, #output
    same_value_validator,
    same_type_validator,
    in_interval_validator,
    in_range_validator,
    creates_var_validator,
    creates_function_validator

@testset "Validators" begin

    a = (;answer = nothing)
    @test  same_expression_validator("x + 2")("x+2", a, nothing).correct
    @test !same_expression_validator("x + 3")("x+2", a, nothing).correct

    @test  has_expression_validator("exp(_)")("tanh(exp(-x))*x", a, nothing).correct
    @test !has_expression_validator("exp(x)")("tanh(exp(-x))*x", a, nothing).correct

    @test  match_input_validator(r"sin")("sin(2x)", a, nothing).correct
    @test !match_input_validator(r"sin")("cos(2x)", a, nothing).correct

    @test  match_output_validator(r"brown")("XXX", a,
                                           (;result="The quick brown fox...")).correct
    @test !match_output_validator(r"brown")("XXX", a,
                                            (;result="Four score and seven...")).correct

    @test  same_value_validator(42)("XXX", a, (;result=42)).correct
    @test !same_value_validator(42)("XX", a, (;result=41)).correct

    @test  same_type_validator(Integer)("XXX", a, (;result=42)).correct
    @test !same_type_validator(Integer)("XXX", a, (;result=42.0)).correct

    @test  in_interval_validator((0,1))("XXX", a, (;result=1/2)).correct
    @test !in_interval_validator((0,1))("XXX", a, (;result=-1/2)).correct

    @test  in_range_validator(Set((1,2,3)))("XXX", a, (;result=1)).correct
    @test !in_range_validator(Set((1,2,3)))("XXX", a, (; result=4)).correct

    @test  creates_var_validator(:Base)("XXX", a, (;result=nothing)).correct
    @test !creates_var_validator(gensym())("XXX", a, (;result=nothing)).correct

    λ = x -> sin(x)^2
    vals = (1=>λ(1), 2=>λ(2))
    @test  creates_function_validator(vals)("XXX", a, (;result=λ)).correct
    @test !creates_function_validator(vals)("XXX", a, (;result=λ∘λ)).correct
end
