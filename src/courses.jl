# Improved course management with better question design

function get_courses_dir()
    dir = joinpath(homedir(), ".swirl_julia", "courses")
    mkpath(dir)
    return dir
end

"""
    get_available_courses()

Get all installed courses (built-in and user-installed).
"""
function get_available_courses()
    courses = Course[]

    # Load built-in courses
    push!(courses, create_basic_julia_course())

    # Load user-installed courses from courses directory
    courses_dir = get_courses_dir()

    for entry in readdir(courses_dir, join=false)
        course_path = joinpath(courses_dir, entry)

        # Skip if not a directory
        if !isdir(course_path)
            continue
        end

        # Look for course.jl in the directory
        course_file = joinpath(course_path, "course.jl")

        if isfile(course_file)
            try
                # Load the course by including the file
                course = include(course_file)

                # Verify it's a Course object
                if isa(course, Course)
                    push!(courses, course)
                else
                    @warn "File $course_file did not return a Course object"
                end
            catch e
                @warn "Failed to load course from $course_file: $e"
            end
        end
    end

    return courses
end

"""
    install_course(source)

Install a course from a local path, URL, or GitHub repository.

# Examples
```julia
# From local directory
install_course("/path/to/my_course")

# From URL (downloads .zip or .tar.gz)
install_course("https://example.com/course.zip")

# From GitHub repo
install_course("https://github.com/user/swirl-course")
```

The source must contain a `course.jl` file that returns a Course object.
"""
function install_course(source::String)
    courses_dir = get_courses_dir()

    # Determine source type
    if isdir(source)
        # Local directory
        return install_course_from_local(source, courses_dir)
    elseif startswith(source, "http://") || startswith(source, "https://")
        # URL or GitHub
        return install_course_from_url(source, courses_dir)
    else
        println("❌ Error: Source must be a local directory path or URL")
        println("Examples:")
        println("  install_course(\"/path/to/course\")")
        println("  install_course(\"https://github.com/user/repo\")")
        return false
    end
end

"""
    install_course_from_local(source_path, courses_dir)

Install a course from a local directory by copying it.
"""
function install_course_from_local(source_path::String, courses_dir::String)
    # Check for course.jl
    course_file = joinpath(source_path, "course.jl")

    if !isfile(course_file)
        println("❌ Error: No course.jl found in $source_path")
        println("A valid course directory must contain a course.jl file that returns a Course object.")
        return false
    end

    # Try to load the course to validate it
    try
        course = include(course_file)

        if !isa(course, Course)
            println("❌ Error: course.jl did not return a Course object")
            return false
        end

        # Sanitize course name for directory
        safe_name = replace(course.name, r"[^a-zA-Z0-9_-]" => "_")
        dest_path = joinpath(courses_dir, safe_name)

        # Check if already installed
        if isdir(dest_path)
            print("Course '$(course.name)' is already installed. Overwrite? (yes/no): ")
            response = lowercase(strip(readline()))
            if response != "yes" && response != "y"
                println("Installation cancelled.")
                return false
            end
            # Remove old version
            rm(dest_path, recursive=true, force=true)
        end

        # Copy the course directory
        cp(source_path, dest_path)

        println("✓ Successfully installed course: $(course.name)")
        println("  Location: $dest_path")
        println("  Lessons: $(length(course.lessons))")

        return true

    catch e
        println("❌ Error loading course: $e")
        return false
    end
end

"""
    install_course_from_url(url, courses_dir)

Install a course from a URL (GitHub or direct download).
"""
function install_course_from_url(url::String, courses_dir::String)
    println("📦 Downloading course from $url...")

    # Create temporary directory
    temp_dir = mktempdir()

    try
        if contains(url, "github.com")
            # GitHub repository
            return install_course_from_github(url, courses_dir, temp_dir)
        else
            # Direct download
            return install_course_from_direct_url(url, courses_dir, temp_dir)
        end
    catch e
        println("❌ Error downloading course: $e")
        rm(temp_dir, recursive=true, force=true)
        return false
    end
end

"""
    install_course_from_github(url, courses_dir, temp_dir)

Install a course from a GitHub repository.
"""
function install_course_from_github(url::String, courses_dir::String, temp_dir::String)
    # Convert GitHub URL to downloadable zip URL
    # https://github.com/user/repo -> https://github.com/user/repo/archive/refs/heads/main.zip

    url = rstrip(url, '/')

    if endswith(url, ".git")
        url = url[1:end-4]
    end

    # Try main branch first, then master
    for branch in ["main", "master"]
        zip_url = "$url/archive/refs/heads/$branch.zip"

        println("Trying branch: $branch")

        zip_file = joinpath(temp_dir, "course.zip")

        # Download using Julia's download function
        try
            download(zip_url, zip_file)
        catch
            if branch == "master"
                println("❌ Could not download from main or master branch")
                return false
            end
            continue
        end

        # Extract zip
        println("Extracting...")

        # Use system unzip or Julia's tar (for .tar.gz)
        try
            run(`unzip -q $zip_file -d $temp_dir`)
        catch
            println("❌ Error: unzip command not found. Please install unzip or extract manually.")
            return false
        end

        # Find the extracted directory (usually repo-name-branch)
        extracted_dirs = filter(d -> isdir(joinpath(temp_dir, d)) && d != "course.zip",
            readdir(temp_dir))

        if isempty(extracted_dirs)
            println("❌ No directory found in extracted archive")
            return false
        end

        extracted_path = joinpath(temp_dir, extracted_dirs[1])

        # Now install from the extracted directory
        return install_course_from_local(extracted_path, courses_dir)
    end

    return false
end

"""
    install_course_from_direct_url(url, courses_dir, temp_dir)

Install a course from a direct download URL (.zip or .tar.gz).
"""
function install_course_from_direct_url(url::String, courses_dir::String, temp_dir::String)
    # Determine file type
    if endswith(url, ".zip")
        archive_file = joinpath(temp_dir, "course.zip")
        extract_cmd = `unzip -q $archive_file -d $temp_dir`
    elseif endswith(url, ".tar.gz") || endswith(url, ".tgz")
        archive_file = joinpath(temp_dir, "course.tar.gz")
        extract_cmd = `tar -xzf $archive_file -C $temp_dir`
    else
        println("❌ Unsupported archive format. Please use .zip or .tar.gz")
        return false
    end

    # Download
    println("Downloading...")
    try
        download(url, archive_file)
    catch e
        println("❌ Download failed: $e")
        return false
    end

    # Extract
    println("Extracting...")
    try
        run(extract_cmd)
    catch e
        println("❌ Extraction failed: $e")
        println("Make sure unzip/tar commands are available")
        return false
    end

    # Find the course directory
    extracted_dirs = filter(d -> isdir(joinpath(temp_dir, d)) &&
            !startswith(d, "course."),
        readdir(temp_dir))

    if isempty(extracted_dirs)
        println("❌ No directory found in extracted archive")
        return false
    end

    extracted_path = joinpath(temp_dir, extracted_dirs[1])

    # Install from extracted directory
    return install_course_from_local(extracted_path, courses_dir)
end

"""
    uninstall_course(name)

Uninstall a course by name.

# Example
```julia
uninstall_course("My Custom Course")
```

Note: Built-in courses (like "Julia Basics") cannot be uninstalled.
"""
function uninstall_course(name::String)
    # Check if it's a built-in course
    if name == "Julia Basics"
        println("❌ Cannot uninstall built-in course: $name")
        return false
    end

    courses_dir = get_courses_dir()
    safe_name = replace(name, r"[^a-zA-Z0-9_-]" => "_")
    course_path = joinpath(courses_dir, safe_name)

    # Check if course exists
    if !isdir(course_path)
        println("❌ Course '$name' is not installed")
        println("\nInstalled courses:")
        list_installed_courses()
        return false
    end

    # Confirm deletion
    print("Are you sure you want to uninstall '$name'? (yes/no): ")
    response = lowercase(strip(readline()))

    if response != "yes" && response != "y"
        println("Uninstall cancelled.")
        return false
    end

    # Delete progress for this course
    try
        reset_course_progress(name)
        println("✓ Removed progress data")
    catch
        # Progress might not exist, that's okay
    end

    # Remove course directory
    try
        rm(course_path, recursive=true, force=true)
        println("✓ Successfully uninstalled course: $name")
        return true
    catch e
        println("❌ Error removing course: $e")
        return false
    end
end

"""
    list_installed_courses()

List all installed courses (both built-in and custom).
"""
function list_installed_courses()
    courses = get_available_courses()

    if isempty(courses)
        println("No courses installed.")
        return
    end

    println("\n📚 Installed Courses:")
    println("="^60)

    for course in courses
        is_builtin = (course.name == "Julia Basics")
        builtin_marker = is_builtin ? " [Built-in]" : " [Custom]"

        println("\n$(course.name)$builtin_marker")
        println("  $(course.description)")
        println("  Lessons: $(length(course.lessons))")

        # Show lesson names
        for (i, lesson) in enumerate(course.lessons)
            println("    $i. $(lesson.name)")
        end
    end
    println()
end

"""
    list_courses()

List all available courses with their descriptions.
"""
function list_courses()
    courses = get_available_courses()

    println("\nAvailable Swirl Courses:")
    println("="^60)

    for course in courses
        println("\n📚 $(course.name)")
        println("   $(course.description)")
        println("   Lessons: $(length(course.lessons))")
    end
    println()
end

"""
    create_basic_julia_course()

Create the built-in Julia Basics course with fundamental lessons.
"""
function create_basic_julia_course()
    # Lesson 1: Basic Math and Bindings
    lesson1 = Lesson(
        "Basic Math and Bindings",
        md"# Basic Math and Bindings",
        md"## Learn basic arithmetic operations and how to create bindings in Julia.",
        [
            Question(
                md"*Welcome to Swirl* for `Julia`! 
In this lesson, you'll learn the basics of `Julia` programming. We'll start with simple math operations and bindings. Let's begin! `Julia` can be used as a calculator. Try adding `5 + 3`.",
                :code,
                8,
                md"Simply type the numbers and the plus sign: `5 + 3`
`Julia` will evaluate the expression and show you the result."
            ),
            Question(
                md"**Great**! Now try multiplication. What is `7 * 6`?",
                :code,
                42,
                md"In `Julia`, multiplication uses the asterisk symbol: `*`
Type: `7 * 6`"
            ),
            Question(
                md"`Julia` uses `^` for exponentiation. Calculate `2` raised to the power of `8`.",
                :code,
                256,
                md"The `^` symbol (caret) means 'to the power of' 
Type: `2^8`
This means 2 × 2 × 2 × 2 × 2 × 2 × 2 × 2"
            ),
            Question(
                md"Now let's learn about bindings. You can think of bindings as assigning names to values using the `=` operator. Create a binding called `x` to the value `10`.",
                :code,
                10,
                md"Bindings let you refer to values for later use.
Type: `x = 10`
The binding name goes on the left, equals sign in the middle, value on the right."
            ),
            Question(
                md"**Good**! Bindings let you reuse values. Now create a binding `y` with the value `5`, then add `x` and `y` together.",
                Val(:multistep_code),
                15,
                "",
                [
                    md"Create a binding `y` with value `5`.",
                    md"Add `x` and `y` together."
                ],
                [
                    md"Type: `y = 5`
Bindings let you refer to values for later use.",
                    md"Type: `x + y`
Remember: `x` is still `10` from the previous question!"
                ],
                "x = 10"
            ),
            Question(
                md"**Excellent**! Bindings can be reassigned. Set `x` to be `x * 2` (which should give `20`).",
                :code,
                20,
                md"You can reassign a binding, even using its old value.
Type: x = x * 2
This takes the current value of x (10), multiplies it by 2, and binds the name x to the result (20).
This is like saying *x becomes x times 2*",
                "x = 10"
            ),
            Question(
                md"**Perfect**! You've learned basic math operations and bindings in Julia.",
                :message,
                nothing
            )
        ]
    )

    # Lesson 2: Types and Functions
    lesson2 = Lesson(
        "Types and Functions",
        md"# Types and Functions",
        md"## Learn about Julia's type system and how to use functions.",
        [
            Question(
                md"# Type inspection
`Julia` is a dynamically typed language, but types are very important. You can check the type of any value using the `typeof()` function. 
Try it out: use `typeof()` to find out what type the number `42` is.",
                :code,
                Int64,
                md"# Functions  
Functions in `Julia` are called by putting the argument in parentheses. 
Type: `typeof(42)`
The `typeof()` function tells you what kind of data you have.
`42` is a whole number, so `Julia` stores it as `Int64` (64-bit integer)."
            ),
            Question(
                md"Now check the type of `3.14` (a decimal number).",
                :code,
                Float64,
                md"Numbers with decimal points are different from whole numbers.
Type: `typeof(3.14)`
`Julia` stores decimal numbers as `Float64` (64-bit floating-point number).
This is why `42` and `3.14` have different types!"
            ),
            Question(
                md"Strings in Julia are created with double quotes. Check what type `\"hello\"` is.",
                :code,
                String,
                md"Text in programming is called a 'string' (a string of characters). Type: `typeof(\"hello\")`
Important: Use double quotes (\"), not single quotes (`\'`).
In `Julia`, `\"hello\"` is a `String`, but 'h' would be a Char (single character)."
            ),
            Question(
                md"`Julia` has many built-in functions. The `sqrt()` function calculates square roots.
Calculate the square root of `16`.",
                :code,
                4.0,
                md"The square root of a number `n` is a value that, when multiplied by itself, gives `n`.
Type: `sqrt(16)`.
Since `4 × 4 = 16`, the square root of `16` is `4`.
Note: The result will be `4.0` (a `Float`), not `4` (an `Int`)."
            ),
            Question(
                md"The `abs()` function returns the absolute value (removes the negative sign). What is the absolute value of -5?",
                :code,
                5,
                md"The absolute value is the distance from zero, ignoring direction.
Type: `abs(-5)`.
`abs()` removes the negative sign, so `-5` becomes `5`.
Think of it as asking 'how far from zero?' (`-5` is `5` units away)."
            ),
            Question(
                md"You can define your own functions! For simple functions, use this syntax:
`functionname(parameter) = expression`
Creating a function has three parts:
1. Function name: `double`
2. Parameter (input): `x`
3. What it does: multiply by `2`

Create a function called `double` that takes one number and returns twice its value. 
Then test it by calling `double(3)`.",
                Val(:multistep_code),
                6,
                "",
                [
                    md"Define the function `double` that returns twice its input",
                    md"Call `double(3)` to test it."
                ],
                [
                    md"Type: `double(x) = 2x`
This creates a function named `double` that takes one argument `x` and returns `2` times `x`.
In Julia, you can write `2x` instead of `2*x` for multiplication!
Type: `double(3)`
This should return `6`, since `double(3) = 2 * 3`."
                ]
            ),
            Question(
                md"**Great job**! You now understand types and functions in `Julia`.",
                :message,
                nothing
            )
        ]
    )

    # Lesson 3: Vectors and Arrays
    lesson3 = Lesson(
        "Vectors and Arrays",
        md"# Vectors and Arrays",
        md"## Learn how to work with vectors and arrays, `Julia`'s fundamental data structures.",
        [
            Question(
                md"Arrays are collections of values. 
In `Julia`, you create a vector (1D array) using square brackets with their members being separated by commas, as in `[1,2,3,4,5]`.
Create a vector with the numbers 10, 20, 30, 40, 50.",
                :code,
                [10, 20, 30, 40, 50],
                md"Arrays let you store multiple values under one name.
Type: `[10, 20, 30, 40, 50]`
Use square brackets `[` `]` with commas separating the values.
The order matters: `[10, 20, 30, 40, 50]` is different from `[50, 40, 30, 20, 10]`!"
            ),
            Question(
                md"You can access elements of an array using square brackets. 
`Julia` uses 1-based indexing (the first element is at index 1).
First, create a vector: `v = [10, 20, 30]`.
Then access its first element: `v[1]`.",
                Val(:multistep_code),
                10,
                "",
                [
                    md"Create the vector `v` with values `10`, `20`, `30`",
                    md"Access the first element of `v`"
                ],
                [
                    md"Accessing array elements uses the syntax: `arrayname[index]`
Type: `v = [10, 20, 30]`\nThis creates a vector named `v` containing the numbers `10`, `20`, and `30`.",
                    md"Type: `v[1]`
This retrieves the first element of `v`, which is `10`.
Remember: `Julia` counts from `1`, not `0`!
"
                ]
            ),
            Question(
                md"The `length()` function tells you how many elements are in an array.
How many elements does `v` have?
Note: If you’ve just started a new `REPL` session, you’ll need to define `v` again. Recall that you assigned `v = [10, 20, 30]` in the previous question.",
                :code,
                3,
                md"The `length()` function counts how many items are in an array.
Type: `length(v)`
Remember: `v` was created in the previous question as `[10, 20, 30]`
So it has 3 elements: `10`, `20`, and `30`.",
                "v = [10, 20, 30]"
            ),
            Question(
                md"You can use the range operator `:` to create sequences, then `collect()` to make a vector.
Create a vector containing all integers from `1` to `10`.",
                :code,
                collect(1:10),
                md"The colon `:` creates a range (a sequence of numbers).
Type: `collect(1:10)`
Breaking it down:
- `1:10` means 'from 1 to 10'
- `collect()` converts the range into an actual array
This creates `[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]` automatically!"
            ),
            Question(
                md"Julia has many array functions. The `sum()` function adds all elements together.
What is the sum of the array `[1, 2, 3, 4]`?",
                :code,
                10,
                md"The `sum()` function adds up all the numbers in an array.
Type: `sum([1, 2, 3, 4])`
This calculates: `1 + 2 + 3 + 4 = 10`
It's a shortcut so you don't have to add them manually!
Type: `sum([1, 2, 3, 4])`
This calculates: `1 + 2 + 3 + 4 = 10`
It's a shortcut so you don't have to add them manually!"
            ),
            Question(
                md"You can add one or more elements to the end of an array using `push!()`. The function takes the array as its first argument and the element(s) to be added as the next argument(s) separated by commas.
Add the number `6` to the end of `[1, 2, 3, 4, 5]`.",
                :code,
                [1, 2, 3, 4, 5, 6],
                md"The `push!()` function adds an item to the end of an array.
Type: `push!([1, 2, 3, 4, 5], 6)`
The syntax is: `push!(array, new_element)`
The exclamation mark `!` means it modifies the array.
This will turn `[1, 2, 3, 4, 5]` into `[1, 2, 3, 4, 5, 6]`"
            ),
            Question(
                md"**Excellent**! You've learned the basics of working with arrays in `Julia`.",
                :message,
                nothing
            )
        ]
    )

    return Course(
        "Julia Basics",
        "An introduction to Julia programming covering basic syntax, types, functions, and arrays.",
        [lesson1, lesson2, lesson3]
    )
end