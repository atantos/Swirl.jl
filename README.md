# Swirl.jl 🌀

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Learn `Julia` interactively in your console/terminal, inspired by the [swirl](https://swirlstats.com/) library for `R`!

## Overview

`Swirl.jl` is an interactive learning platform that teaches you `Julia` programming right in your `REPL` (Read-Eval-Print Loop). Work through lessons at your own pace, get instant feedback on your answers, and build your `Julia` skills step by step.

After trying it out, feel free to give it a ⭐ on GitHub!

## ✨ Features

- 🎓 **Interactive Learning**: Type real `Julia` code and get immediate feedback
- 📚 **Structured Courses**: Progress through well-designed lessons
- 💾 **Auto-Save Progress**: Your progress is automatically saved after each question
- 💡 **Smart Hints**: Get detailed, educational hints when you're stuck
- 🎯 **Multiple Question Types**: Messages, multiple choice, and code evaluation
- 🌟 **Built-in Content**: Comes with a short "Julia Basics" course
- 📦 **Custom Courses**: Install courses from `GitHub`, URLs, or local directories
- 🔄 **Flexible Navigation**: Easy menu navigation with back/exit commands
- 🎮 **Progress Management**: Reset and retake lessons anytime
- 🚀 **Natural Interaction**: Multi-step questions and natural code exploration

## Installation

```julia
# From Julia REPL
using Pkg
Pkg.add(url="https://github.com/atantos/Swirl.jl")
```

Or for local development:

```julia
using Pkg
Pkg.develop(path="/path/to/Swirl.jl")
```

## Quick Start

```julia
using Swirl

# Start learning!
swirl()
```

That's it! The interface will guide you through:

1. Selecting a course
2. Choosing a lesson
3. Working through interactive questions

## 📖 Usage

### Main Commands

```julia
# Start an interactive lesson
swirl()

# View all available courses
list_courses()

# Install a custom course
install_course("path/or/url")

# Uninstall a custom course
uninstall_course("Course Name")

# Delete all progress
delete_progress()
```

### During a Lesson

While working through a lesson, you can use these commands:

| Command          | Action                                                   |
| ---------------- | -------------------------------------------------------- |
| `hint` or `?`    | Get a detailed hint for the current question             |
| `skip`           | Skip the current question and move to the next           |
| `back` or `menu` | Return to the lesson selection menu                      |
| `info`           | Show all available commands                              |
| `exit` or `quit` | Exit `Swirl` (returns to `Julia` `REPL`, progress saved) |

### Navigation

**At the Course Menu:**

- Type a number to select a course
- Type `0` to exit `Swirl`

**At the Lesson Menu:**

- Type a number to select a lesson
- Type `0` to go back to course selection
- Type `reset 1` to reset lesson 1
- Type `reset all` to reset all lessons in the course

## 📚 Example Session

```julia
julia> using Swirl

julia> swirl()

============================================================
| Welcome to Swirl for Julia! 🌀
============================================================

🌀 Type ')' to enter Swirl mode!
   (Press backspace anytime to exit Swirl mode)

Available courses:
  1. Julia Basics
 -1. Exit Swirl

💡 Select a course (enter number):

swirl> 1

============================================================
Lessons in Julia Basics:
============================================================
  1. [ ] Basic Math and Bindings
  2. [ ] Types and Functions
  3. [ ] Vectors and Arrays

Commands:
  0. Back to course selection
 -1. Exit Swirl
  reset <number> - Reset a specific lesson (e.g., 'reset 1')
  reset all - Reset all lessons in this course

💡 Type a lesson number or command:

swirl> 1

============================================================
| Basic Math and Bindings
============================================================
Learn basic arithmetic operations and how to create bindings in Julia.

--- Question 1 of 7 ---

Welcome to Swirl for Julia! In this lesson, you'll learn the basics of
Julia programming. We'll start with simple math operations and bindings.

Let's begin! Julia can be used as a calculator. Try adding 5 + 3.

swirl> 5 + 3
✓ Correct!

--- Question 2 of 7 ---

Great! Now try multiplication. What is 7 * 6?

swirl> hint
💡 Hint: In Julia, multiplication uses the asterisk symbol: *
Type: 7 * 6
(Note: Unlike some languages, you can't skip the * symbol, so '7 6' won't work)

swirl> 7 * 6
✓ Correct!

...
```

> **Note:** The example above shows REPL mode (with ReplMaker.jl installed). The prompt changes to `swirl>` and you get syntax highlighting. Without ReplMaker, you'll use classic mode with the standard `julia>` prompt.

## 📦 Course Structure

### Built-in: `Julia` Basics

`Swirl.jl` comes with the **`Julia` Basics** course, which includes:

#### 1. Basic Math and Bindings

- Arithmetic operations (+, -, \*, /, ^)
- Biding creation and naming
- Updating bindings

#### 2. Types and Functions

- Understanding `Julia`'s type system
- Using `typeof()` for type checking
- Built-in functions (sqrt, abs, etc.)
- Defining your own simple functions

#### 3. Vectors and Arrays

- Creating arrays with `[]`
- Array indexing (1-based)
- Array functions (length, sum, push!, etc.)
- Using ranges with `:`

Each lesson includes:

- Clear explanations
- Hands-on exercises
- Detailed hints
- Multiple choice questions
- Progress tracking

## 🎓 Installing Custom Courses

### From a Local Directory

```julia
# Your course directory should contain a course.jl file
install_course("/path/to/my_course")
```

### From GitHub

```julia
# Install directly from a GitHub repository
install_course("https://github.com/atantos/course-repo")
```

### From a URL

```julia
# Install from a .zip or .tar.gz archive
install_course("https://example.com/course.zip")
```

> **Note:** Installing from GitHub or URLs requires system commands to be available:
> - `unzip` for .zip files (GitHub repos and .zip archives)
> - `tar` for .tar.gz files
> 
> These are typically pre-installed on Linux/macOS. On Windows, you may need Git Bash, WSL, or manual extraction.

### Example

```bash
# 1. Create a course directory
mkdir ~/my_julia_course

# 2. Copy the template
cp templates/course/course.jl ~/my_julia_course/

# 3. Edit it with your content
# Edit ~/my_julia_course/course.jl

# 4. Install it
```

```julia
using Swirl
install_course(expanduser("~/my_julia_course"))

# 5. Use it!
swirl()
```

## 📝 Creating Custom Courses

### Quick Start

1. **Use the template:**

   ```bash
   cp templates/course/course.jl ~/my_course/
   ```

2. **Edit the course:**

   ```julia
   Course(
       "My Awesome Course",
       "Description of what students will learn",
       [
           Lesson("Lesson 1", "Description", [
               Question("Welcome!", :message, nothing),
               Question("Calculate 1+1", :code, 2, "Type: 1+1"),
               # ... more questions
           ]),
           # ... more lessons
       ]
   )
   ```

3. **Install and test:**
   ```julia
   install_course(expanduser("~/my_course"))
   swirl()
   ```

### Question Types

**Message (Information Only):**

```julia
Question("Welcome to this lesson!", :message, nothing)
```

**Code Execution:**

```julia
Question(
    "Calculate 2 + 2",
    :code,
    4,              # Expected result
    "Type: 2 + 2"   # Hint
)
```

**Multiple Choice:**

```julia
# Helper function (recommended)
mc(q, choices, correct_idx, hint="") =
    Question(q, :multiple_choice, correct_idx, hint, choices, nothing)

mc(
    "What is Julia?",
    ["A programming language", "A person", "A city"],
    1,              # Correct answer (1-based indexing)
    "It's for programming!"
)

# Or use the full constructor directly:
Question(
    "What operator is used for exponentiation?",
    :multiple_choice,
    3,                      # Index of correct answer
    "It's the ^ symbol",    # Hint
    ["*", "**", "^", "pow"], # Choices array
    nothing                 # Validator (usually nothing)
)
```

**Multi-Step Code:**

```julia
Question(
    "Create x = 5, then calculate x * 2",
    :code,
    10,                    # Final result
    "Type: x = 5; x * 2"   # Hint
)
```

## 📚 Package Structure

```
Swirl.jl/
├── src/                    # Source code
│   ├── Swirl.jl           # Main module
│   ├── types.jl           # Core data structures
│   ├── parser.jl          # Code evaluation
│   ├── runner.jl          # Lesson execution
│   ├── progress.jl        # Progress tracking
│   └── courses.jl         # Course management (includes built-in Julia Basics)
│
├── templates/             # Templates for creating courses
│   ├── course/
│   └── lesson/
│
└── docs/                  # Documentation
    └── *.md
```

## 💾 How It Works

### Code Evaluation

`Swirl.jl` evaluates your `Julia` code in real-time using the `Main` module, so:

- ✅ Bindings you create persist between questions
- ✅ You can use any `Julia` feature or package
- ✅ Your code runs in the same environment as your `REPL`
- ✅ Multi-step questions work naturally

### Data Storage

```
~/.swirl_julia/
├── courses/          # Installed custom courses
│   ├── Course1/
│   └── Course2/
└── progress/         # Your saved progress
    └── *.progress
```

Progress is automatically saved:

- After each correct answer
- When you exit a lesson
- When you navigate back to menus

## 💾 How It Works

**Q: Where is my progress saved?**  
A: Progress is saved in `~/.swirl_julia/progress/` in your home directory. It's automatically saved after each question.

**Q: Can I restart a completed lesson?**  
A: Yes! You can either:

- Select the lesson and choose "yes" when asked to restart
- Type `reset 1` at the lesson menu to reset lesson 1

**Q: Will typing 'exit' close my `Julia` session?**  
A: No! The `exit` command returns you to the `Julia` `REPL`. Your session continues and all bindings remain available.

**Q: What if I make a mistake?**  
A: You get 3 attempts per question, and you can always type `hint` for detailed help!

**Q: Can I share my custom course?**  
A: Yes! Push it to `GitHub` and others can install it with:

```julia
install_course("https://github.com/username/your-course")
```

**Q: How do I uninstall a course?**  
A: Use `uninstall_course("Course Name")`. Note: Built-in courses like **`Julia` Basics** cannot be uninstalled.

**Q: Can I modify an installed course?**  
A: Yes! User-installed courses are stored as files in `~/.swirl_julia/courses/` and can be edited directly.

## 🤝 Contributing

Contributions are welcome! Whether it's:

- 🐛 Bug reports
- 💡 Feature suggestions
- 📚 New course content
- 🔧 Code improvements
- 📖 Documentation enhancements

Please feel free to open an issue or submit a pull request.

### Creating a Course to Share

1. Create your course using the templates
2. Test it thoroughly with `install_course()` and `swirl()`
3. Push to `GitHub`
4. Share the installation command!

## 🗺️ Roadmap

- [x] Interactive code evaluation
- [x] Progress tracking and saving
- [x] Multiple question types
- [x] Smart hints system
- [x] Custom course installation (local, `GitHub`, URL)
- [x] Course management (install/uninstall)
- [x] Lesson reset and retake
- [x] Natural multi-step questions
- [x] Improved navigation
- [ ] More built-in courses (intermediate/advanced topics)
- [ ] Multimedia content support
- [ ] Achievements and badges
- [ ] Community course repository
- [ ] Course version management

## 📄 License

MIT License - see LICENSE file for details

## 🙏 Acknowledgments

Inspired by the excellent [swirl](https://swirlstats.com/) package for `R`, which has helped countless people learn `R` programming.

Special thanks to the `Julia` community for creating and maintaining such an amazing language to teach!

---

## 🚀 Get Started Now!

```julia
using Pkg
Pkg.add(url="https://github.com/atantos/Swirl.jl")

using Swirl
swirl()
```

**Happy Learning! 🎉**

Start your `Julia` journey today with `swirl()`!

---

### Quick Commands Reference

```julia
# Learning
swirl()                          # Start learning

# Course Management
list_courses()                   # See all available courses
install_course("source")         # Install course
uninstall_course("Course Name")  # Remove course

# Progress Management
delete_progress()                # Delete all progress

# During Lessons (type these in Swirl mode)
hint          # Get help
skip          # Skip question
menu          # Return to menu
exit          # Exit Swirl
```

That's all you need to get started! 🌀
