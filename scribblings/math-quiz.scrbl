#lang scribble/manual

@(require (for-label math-quiz))

@title{Math Quiz}

@author{Hrvoje Blazevic}

@defmodule[math-quiz]

@centered{@bold{Start the program by clicking on one of Exercise buttons}}

@tabular[#:sep @hspace[6]
         (list
          (list @bold{Button} @bold{Description})
          (list " "          " ")
          (list "÷"          @italic{Starts 3 digit division exercises.})
          (list " "          "________________________________________")
          (list "int or fract ÷"  @italic{Starts division with exact (int/fract) result.})
          (list " "          "________________________________________")
          (list "100/10"     @italic{Starts division table exercises (up to 100/10).})
          (list " "          "________________________________________")
          (list "*"          @italic{Starts multiplication exercises.})
          (list " "          "________________________________________")
          (list "10*10"      @italic{Starts multiplication table exercises (up to 10*10).})
          (list " "          "________________________________________")
          (list "+ -"        @italic{Starts the addition/subtraction exercises.})
          (list " "          "________________________________________")
          (list "< = >"      @italic{Starts the comparison exercises.})
          (list " "          "________________________________________")
          (list "odd even"   @italic{Starts the 3 digit odd or even exercises.})
          (list " "          "________________________________________")
          (list "sequence"   @italic{Starts a missing number in a sequence exercises.})
          (list " "          "________________________________________")
          (list "B B A"      @italic{Starts Before Between After number exercises.})
          (list " "          "________________________________________")
          (list "PosVal"     @italic{Starts Position Value exercises.})
          (list " "          "________________________________________")
          (list "Round"      @italic{Starts number rounding exercises.})
          (list " "          "________________________________________")
          (list "Ordinals"   @italic{Starts ordinal number spelling exercises.})
          (list " "          "________________________________________")
          (list "fractions"  @italic{Starts Fractions exercises.})
          (list " "          "________________________________________")
          (list "clock"      @italic{Starts Clock exercises.})
          (list " "          "________________________________________")
          (list "N->Roman"   @italic{Starts Arabic to Roman number exercises.})
          (list " "          "________________________________________")
          (list "Roman->N"   @italic{Starts Roman to Arabic number exercises.})
          (list " "          "________________________________________")
          (list "cash USD"   @italic{Starts Return Change exercises (USD).})
          (list " "          "________________________________________")
          (list "cash Peso"  @italic{Starts Return Change exercises (Philippine Peso).})
          (list " "          "________________________________________")
          (list "ABC sort"   @italic{Starts Alphabetical Sorting exercises.})
          (list " "          "________________________________________")
          (list "skip+count" @italic{Starts Positive Skip Counting exercises.})
          (list " "          "________________________________________")
          (list "skip-count" @italic{Starts Negative Skip Counting exercises.})
          (list " "          "________________________________________")
          (list "missing X"  @italic{Starts Find the missing Operand exercises.})
          (list " "          "________________________________________")
          (list "operators"  @italic{Starts missing operators (+ -) exercises.})
          (list " "          "________________________________________")
          (list "time"       @italic{Starts elapsed time exercises.})
          (list " "          "________________________________________")
          (list "GAPESA"     @italic{Given, Asked, Process, Equation, Solution, Answer})
          (list " "          "________________________________________")
          (list "Perimeter/Area" @italic{Starts circumference/area related exercises.}))]


@section{Number of Exercises}

Default number of exercises given in each choice above is 20, but you can change this
in @bold{Setup->Set number of exercises} menu option to 1-30 exercises. When you click
on this menu, the slider window will open. Move the slider to desired value and close
the slider window by clicking X in the top right corner. When one of the exercises is
running, the @bold{Setup} menu is disabled until the exercises are completed.
Likewise, while a set of exercises is running, all start exercise buttons are disabled.

@section{Arithmetic Exercises}

Once you start the arithmetic (+ -, *, integer ÷, ÷, 10*10, 100/10) exercises, enter the
result of requested exercise in the text field, and click @bold{Calculate} button. Instead of
clicking the @bold{Calculate} button, you can also hit @bold{enter} key on keyboard.
If the result was correct, the confirmation will be printed in black in the panel below.
If, on the other hand, the result was not correct, the same line will be printed in red.

@subsection{Arithmetic levels +-}

Plus and minus exercises start by default at level 1; two digit addition only, with no
carry over. This can be changed with @bold{Setup->Set + - difficulty level} sub-menu.
Level 1 is 2 digit limited addition, level 2 is 2 digit limited mixed addition/subtraction,
level 3 is 3 digit unlimited addition/subtraction, level 4 is subtraction with negative
result. Level 5 is simple fraction (equal denominators) addition/subtraction, level 6 is
one denominator divisible by other denominator addition/subtraction. Level 7 is both
denominators divisible with the same number, level 8 is denominators not divisible,
and level 9 is mixed exercises from level 6,7, and 8.
Level 0 is mixed addition/subtraction, of numbers up to 18,
designed to teach kids fast calculation without using fingers for counting.

@subsection{Arithmetic levels *}

Multiplication exercises start by default at level 2, which is 3 digit by 3 digit multiplication.
This can be changed with @bold{Setup->Set * level: 1, multiplies of 10; 2, 3d*; 3, fractions}
sub-menu. Level 1, uses multiples of 10 as a factor, and level 3 multiplies fractions.

@subsection{Arithmetic levels Integer/fraction division}

Integer/fraction division exercises start by default at level 1; one digit divisor.
This can be changed with @bold{Setup->Set integer/fraction ÷ difficulty level} sub-menu.
Level 2 is 1 digit divisor with remainder. Level 3 is 2 digit divisor, but observing the max allowed dividend (as set by
@bold{max size of numbers} sub-menu). Level 4 uses 2 digit divisor, but requires remainder.
Level 5 uses 2 digit divisor, but will exceed the max size of numbers. Level 6 requires
remainder. Level 7 is fraction division.

@subsection{Numeric Precision}

All arithmetic exercises require exact (integer or fraction) results. Only exceptions are
division (÷) that requires almost always inexact, (real -- floating point) numbers, but
limited only to 2 digits after the decimal point. No need to enter more - excess digits will
be discarded! You can change the default setting of 2 digits after decimal point in
@bold{Setup->Set division precision} menu. The pop-up slider allows 0-7 digit setting.
Having said that, of-course, if division exercise actually gives an integer result
(as in 10 / 2 = 5) entering only 5 as a result is fine. Likewise if result has less
than 2 digits after decimal point, as in (7 / 2 = 3.5), entering only 3.5 is okay.
The other is integer division (÷) where all even levels (2 4 6) require result to be entered
as: 12r3 (where the problem was stated as 63÷5). 

@subsection{Maximum Factor}

Default maximum factor (for both multiplication and division tables) is set to 10, but
for a beginner student, this can be changed with @bold{Setup->Set max factor for * ÷ table}.
Slider will go from 5 (5*5) to 12 (12*12), but the default value is 10. If you set it to 12,
then consequently, division table works from 144/12.
The rationale for starting with (5*5), is for a young student to start easy, and then
gradually increase difficulty by changing up to (12*12).

@subsection{Maximum Size of Numbers}

To change the max size of numbers (for most exercises, except for those where it obviously
does not make sense, like Roman numbers) click on @bold{Setup->Set max size of numbers} menu.
Move the slider to desired size. Slider goes from 100 to 900 with default of 700.
It is recommended to lower the number to say, max 350, to avoid huge results in (*) exercises.


@section[#:tag "comparison"]{Comparison Exercises}

The comparison exercise @bold{<=>}, works slightly differently. It opens a separate input window,
and of-course does not require numerical result. Student is asked to input one of comparison
symbols @bold{> = <} depending on the two numbers shown to the left and the right of the input
field, as in: 133  [     ]  211 . In this case the student should type @bold{<} in the input
field, as 133 is smaller than 211. 

@subsection{Comparison Levels}

Comparison exercise works in 3 levels. Level one (integer comparison, described above)
is the default level. This can be changed with
@bold{Setup->Set comparison type: integer or fraction} menu to compare fractions,
as in 3/4 [ ] 3/5. Level 2 is easy fractions (having the same numerators or
denominators). Level 3 is using different numerators and denominators.

@subsection{Restoring the Accidentally Closed or Hidden Window}

In case that the student accidentally closes the comparison input window (clicking the X
on the title-bar of the window), before the set of exercises is completed, then click on
the @bold{Lost and Found->Show Comparison Window} menu, and the window will pop up
again in the same state as when it was closed.

All of the exercises described below, also have an entry in the
@bold{Lost and Found->Show ****** Window} menu, so this will not be mentioned again.

@section{Odd/Even Exercises}

The odd/even exercise works also differently, but the input pop-up window is self
explanatory. It displays the number, and two radio buttons labeled odd, and even.
If the number is odd, click odd radio button, and vice versa. Then click @bold{Check} button
to send your choice to be evaluated. Hitting @bold{enter} key also works.

@section{Sequence Exercises}

The sequence exercise also pops-up a window with sequence in question shown as following:
1  2  3  [  ]  5 . In this case the student should enter 4 in the input field and
click on @bold{Check} button, or just hit @bold{enter} key.

@subsection{Sequence Cheat Button}

Additional @bold{Cheat} button is disabled in levels 1,2,3. It is only active in level 4.
Of-course not all sequences are as easy as the one shown here. There are 3(4) levels,
that can be set from the @bold{Setup->Set sequence difficulty level} menu.
Level 1 (which is set as a default) is appropriate for first graders.
Level 4 is just a cheat level, for level 3. It enables the @bold{Cheat} button.
When this button is clicked, it will display the missing number in the input field.
Clicking @bold{Check} after that will verify the result, and display the next question.
Every time that the @bold{Cheat} button is used, this will be confirmed in report printout.

@section{B B A Exercise}

The (B B A) Before, Between, After, exercise pops the window which allows the input
of the requested number. As a help to the student the status line on the bottom of the
window displays what is expected, as in; before, or between, or after.

@section{Position Value Exercise}

The Position value exercise pops up the window with 4 radio buttons labeled ones,
tens, hundreds, and thousands. At the same time one four digit number is displayed with
one of the digits being red. Corresponding to that digit position, the student should
click the appropriate radio button.

@section{Rounding Exercise}

The Rounding exercise pops up the window showing one multi-digit number.
One of the digits is red, and the student should round this number on the
red digit position.

@section{Ordinals Exercise}

The Ordinal number exercise pops up the window showing one (up to 3 digit)
cardinal number. The student should enter the ordinal number of the same 
number, as in 5 enter 5th, 122 enter 122nd. 

@section{Fractions Exercises}

The Fractions exercise pops up the window with pie-chart drawn in it, containing a random
number of red and black slices. The student should enter in input field
red-slices/all-slices, as in: 3/5 (if the drawing contained 3 red and 2 black slices).
No need to enter spaces as in 3 / 5 , but the program can handle that as well.

@subsection{Max Number of Fraction Slices}

The default max size of pie-chart is 10 slices, but that can be changed with
@bold{Setup->Set number of fraction slices} sub-menu. Slider goes from 5-12 slices.

@subsection{Maximum fraction denominator}
Likewise there is a menu entry @bold{Setup->Set maximum fraction denominator} sub-menu,
for changing maximum size of denominator for any exercise that works with fractions:
(non graphical) fraction comparison, addition, subtraction, multiplication, and division
of fractions. The reason for two different choices for basically the same thing
(max denominator), is that graphical display for fractions works well with
up to 12 slices. After that it becomes hard to read, while for the purely numeric
fractions it does not matter. Denominator choice goes from 6 to 18, with 15 being
the default.

@subsection{Fraction Exercise Levels}

Fractions exercise works in 4 levels. Level 1 (reading the fraction, described above)
is the default level. This can be changed with
@bold{Setup->Set fraction level: read or compare} sub-menu to display and compare 2 fractions.
Level 2 works in a similar way to @(secref "comparison") (fractions level). Student should
enter @bold{<, =, >} into the middle input field to indicate weather the left fraction
pie-chart is smaller, equal, or greater than the right one.
Level 3 and 4 work almost the same, except that all 3 input fields are active, and all 3 require
input. Enter left fraction (as in 1/3) into the left field, right fraction into the
right field, and @bold{<, =, >} into the middle field. Level 4 is tougher, because both
numerators and denominators will be different.

@subsection{Fraction Error Reporting}

Error reporting for level 3 is complicated and can be confusing. Program can report
wrong fraction (in either left or right input fields), then wrong comparison input,
and finally if any of the fields has been left blank, the non counting missing input error.

@section{Clock Exercises}

The Clock exercise pops up the window with clock face drawn in it. The student should
enter @bold{hr:mn} in the input field, and click @bold{Check}, or hit @bold{enter} key.

@subsection{Clock Exercise Levels}

There are 5 levels of clock exercises that can be changed with
@bold{Setup->Set clock level: tell time or before/after time}. Default is tell time
(level 1). In level 2-5 (before/after) the student should first read the time on the
clock face, and then enter before or after time, as requested by the prompt.
Level 2 is +/- 60min, level 3 is +/- 60,30min, level 4 is +/- 60,30,15min, and
level 5 is random minutes from 10 to 600 rounded to 10 minutes.

@section{Roman Numerals Exercises}

The default maximum roman number is 100 (to help beginners start with smaller numbers only).
However in the @bold{Setup->Set max Roman number} sub-menu, you can change the default
from minimum 10 to maximum Roman number, which is 3999.

@subsection{N->Roman Exercises}

The N->Roman exercise pops up the window with Arabic number prompt. The student should
enter the equivalent Roman number into the input field. Roman numerals should be entered
in capital letters. However the program will accept lowercase letters without complaint.

@subsection{Roman->N Exercises}

The Roman->N exercise pops up the window with Roman number prompt. The student should
enter the equivalent Arabic number into the input field.

@section{Cash Return Exercises}

The cash (USD) exercise pops a window with price of a purchased item as a prompt.
The window contains 10 input fields (100$, 50$, 20$, 10$, 5$, 1$, quarters, dimes,
nickels, cents).
The status line on the bottom of the window explains that the customer has paid with 500$,
and that the correct amount of cash must be returned. It also explains that entering
more than 4 in any of denomination input fields is counted as an error.
The student should first calculate the correct cash-return (as in 500 - price), and then
enter numbers from 1-4 in denomination input fields, until correct return is reached.
Entering 0 in some input fields that will not be used is not required. Just leave them empty.

@subsection[#:tag "cash"]{Cash Return Error Reporting}

This exercise behaves slightly differently from all previous exercises. It does not clear
the input fields as soon as enter, or check button is clicked. It will clear the inputs
only if the answer was correct, or if the answer was empty (as in, when no numbers are entered).
In case of erroneous input, whether the change is wrong, or the student entered more than
4 in any field, the input fields will not be erased. The student can then find the error,
and correct it.

@subsection{Cash Return - Philippine Peso}

The cash (Philippine Peso) exercise behaves in exactly the same way, except there is no
10 centavos (dime) coin in Philippines. Consequently the @bold{d} input field is
disabled (shaded).

@section{ABC Sort Exercises}

The ABC sort exercise pops the window with 5 English words in random order. Each word has
an input field in front of it. Input the numbers from 1 to 5 in each field corresponding
to the word's alphabetical order.

@subsection{ABC Sort Errors}

Just like the @(secref "cash"), it does not clear the inputs in case of error.
Errors can be of two types. Words were not correctly indexed, or indices did not follow
the rules, as in there are two or more same indices or one or more indices are out of
scope, as in 0,6,7..., or one or more indices were left blank.
Error will be counted only in the first case, when the words were not correctly indexed,
but all indices from 1-5 were used. All the rest of indexing errors will be printed in
the report field in green, and will not be penalized.

@section{Skip Count Exercises}

The @bold{skip+count/skip-count} exercise pops the window with one large input field that has
one (starting) number already in it. Student should continue entering numbers as per
increment/decrement indicated on the prompt after the input field. Comma @bold{(,)} must
separate all the numbers, and student must enter minimum 9 additional numbers.

@subsection{Skip Count increment/decrement}

Default skip increment/decrement is set to 2, but this can be changed with
@bold{Setup->Set max skip-count increment/decrement} sub-menu, with range from 2 to 10.
This, of-course is the absolute value of increment and if negative skip counting exercise
was started, increment will be converted to corresponding negative value.
However to make exercise more interesting, the increment is still randomly chosen,
varying from max-1 (if max>3) or max-2 (if max>6) to max.

@subsection{Skip Count Error Reporting}

Exercise will report 4 possible errors:

@itemlist[@item{Incorrect number(s) starting with X in (n1 n2 n3 @bold{(X)} n4 ...). If one or more numbers
           are wrong. The wrong number will be shown in parentheses.}
          @item{Started with X instead of Y. If student erases the starting number Y, and starts
           counting from their own starting number X.}
          @item{Only X numbers entered! Where X is a number smaller than 10. This error is
           not counted!}
          @item{Erroneous input (x y z...) - error will not be counted! If input is
           syntactically wrong (as in space instead of comma, or any other characters
           entered.}]

Just like with @(secref "cash"), input field will not be cleared on errors.

@section{Missing X Exercises}
The missing X exercise is the missing arithmetic operand exercise. It presents the equation
of the form @bold{X (+-*/) N = M}, or @bold{N (+-*/) X = M}.
The student must answer with the value of @bold{X}. Erroneous answer prints out
the solution equation, just like @(secref "gapesa-error"), and Perimeter/Area exercises.

@subsection{Missing X levels}
There are 3 levels for @bold{missing X} exercise. Level 1 (+ -), level 2 (* /),
and level 3 mix of 1 & 2.

@section{Operators Exercises}
The operators exercise will display an arithmetic term with all operators (+ or -) missing.
Student is asked to enter operators inside input fields, as in: 15 [+] 5 [-] 10 = 10.

@subsection{Operators levels}
There are 3 levels for @bold{operators} exercise: level 1 is one missing operator,
level 2 is two, and level 3 is three missing operators. This can be set with
@bold{Setup->Set missing Operators level} sub-menu.

@section{Time Exercises}
The time exercise is about finding the elapsed time between two or more points.
Requires answer in the form: @bold{5h 23m} (if elapsed time is 323min).
If hour is zero, it still has to be entered as @bold{0h}. No spaces between
5 and h or m are allowed.

@subsection{Time levels}
There are 3 levels for @bold{time} exercise.
Level 1 is easy, level 2 a bit more complicated, and level 3 is a mix of previous 2 levels.

@section[#:tag "gapesa"]{GAPESA Exercises}
The GAPESA exercise is math problem given in words. It stands for Given, Asked, Process,
Equation, Solution, and Answer.
It pops a window with the text of a problem. The student must write the equation on a
scratch-pad, calculate the solution and answer it in the input field.

@subsection{GAPESA Levels}

There are 10 levels of this exercise. Level 1 (the default) is addition only, level 2 is
subtraction only. Level 3 is a mix of addition and subtraction problems, chosen at random.
Level 4 is addition and subtraction combined in one exercise, level 5 is all levels
of (+ - +-) combined.
Level 6 is multiplication problems, level 7 is easy (1 digit) division problems,
level 8 is a mix of multiplication and easy division problems, 
level 9 is division problems, and level 10 is multiplication and division combined.
This can be set with @bold{Setup->Set GAPESA level} sub-menu.

@subsection{GAPESA Precision}

If the student is working on division exercises, then precision of the answer becomes an issue.
Just like in arithmetic division problems, the number of significant (taken into account)
decimal places can be set with @bold{Setup->Set Division Precision} sub-menu. Default
is 2 places. Do not round up the answer, just give 2 decimal digits (or whatever is
set up for division precision) without rounding the number.
However in existing set of exercises, only 2 problems require inexact (floating point)
number. All the others are set up so, that the answer is always an integer, except in exercises
that require a fraction as a result. In that case just input 1/3 (if this is the correct
answer). Do not use spaces as in 1 / 3, because 1/3 is a number (fraction),
and 1 / 3 is @bold{not} a number!

@subsection[#:tag "gapesa-error"]{GAPESA Error Reporting}

In case the student answers with wrong solution (wrong number), error message will be
printed in red, but it will also show the correct equation for the solution.

@section{Perimeter/Area Exercises}

The Perimeter/Area exercises deal with calculation of circumference and area of several
different types of polygons, including the circle (in higher levels).
The exercise behaves exactly the same way as the @(secref "gapesa") explained above,
except for the levels, which are 6 here. Level 1, 2, 3, are perimeter exercises,
and levels 4, 5, & 6 are Area exercises.
The levels are chosen with @bold{Setup->Set Perimeter/Area level} sub-menu.
Exercises involving circle require floating point results, therefore the precision (number of
decimal places required) is set with @bold{Setup->Set division precision} sub-menu. 
This is set by default to 2. Again, do not round up/down the answers. 
Circle exercises use @bold{3.14} as the value of @bold{PI} (which is noted in the text of
each exercise). Do not use more precise values for @bold{PI}, because the results will not
be accepted!


@section{Error reporting for ALL Exercises}

For all exercises, if wrong result was entered, the report line (in the report panel)
will be printed in red, and the computer will beep. Program will not move on
(give next problem) until original problem is answered correctly.
This will count as a mistake, and will be penalized after completion of exercises in the
following way: Each mistake will result in one additional exercise, up to 5 mistakes.
No more penalty exercises will be given after 5.

In case that the student does not focus the input field while entering the result,
or he/she inputs a non number, or an invalid symbol for comparison exercises, the reminder
will be printed in green, computer will still beep, but this will not be counted nor
penalized as error.

@section{Time Counting and Penalties for Exceeded Time}

Program is also counting the time that the student is using for exercises. At the onset
of each set of exercises a certain number of minutes is allotted to exercises. Running
time is reported on Status line when each new problem is printed. If the student exceeds
the allowed time, penalty exercises will be given.
The number of penalty exercises is also limited to maximum 5.
This will happen before penalty for mistakes is taken into the account. However all
mistakes will still be counted and penalized after the time penalty is completed.

@subsection{Adjusting the Time for Exercises}

To speed up, or slow down the exercises, give less (or more) time to each exercise open
the @bold{Setup->Set % of inc/dec allotted time}. The slider goes from 50 (%) to 200 (%),
with the latter increasing the time to 200%, while first one decreases the time to 50%.
Default is 100%, which does not change the default values set by the program.
Once this change in speed is made, it remains in place for all future exercises, until
such time you change the speed again using the same sub-menu. To return the speed to
default set by the program, set the slider back to 100%.

@centered{@bold{The default execution times set by the program are:}}

@itemlist[@item{15 seconds per exercise for (+ -) level 0}
          @item{20 seconds per exercise for (+ -) level 1}
          @item{30 seconds per exercise for (+ -) level 2 & 5}
          @item{1.5 minutes per exercise for (+ -) level 3 4 & 6}
          @item{2.5 minutes per exercise for (+ -) level 7 8 & 9}
          @item{30 seconds per exercise for (*) level 1}
          @item{2 minutes per exercise for (*) level 2 & 3}
          @item{2 minutes for integer/fraction ÷ level 1 2 & 7}
          @item{2.5 minutes for integer/fraction ÷ level 3 & 4}
          @item{3 minutes for integer/fraction ÷ level 5 & 6}
          @item{3 minutes per exercise for (÷)}
          @item{30 seconds per exercise for multiplication/division tables}
          @item{20 seconds for integer comparison exercises}
          @item{30 seconds for fraction comparison exercises level 2}
          @item{1.5 minutes for fraction comparison exercises level 3}
          @item{12 seconds for odd/even exercises}
          @item{1 minute for sequence level 1}
          @item{2 minutes for sequence level 2}
          @item{3 minutes for sequence level 3 & 4}
          @item{30 seconds for Before Between After exercises}
          @item{12 seconds for Position Value}
          @item{30 seconds for Rounding numbers}
          @item{15 seconds for Ordinal numbers}
          @item{20 seconds for Fractions level 1}
          @item{40 seconds for Fractions level 2}
          @item{1 minute for Fractions level 3}
          @item{1.5 minutes for Fractions level 4}
          @item{30 seconds for Clock}
          @item{1.5 minutes for Clock before/after time}
          @item{30 seconds for Roman numbers}
          @item{2.5 minutes for cash return exercises}
          @item{2 minutes for ABC-sort exercises}
          @item{2 minutes for skip+count exercises}
          @item{2.5 minutes for skip-count exercises}
          @item{2 minutes for missing X level 1}
          @item{3 minutes for missing X level 2 & 3}
          @item{30 seconds for operators level 1}
          @item{1.5 minutes for operators level 2}
          @item{2.5 minutes for operators level 3}
          @item{3 minutes for time exercises level 1}
          @item{4 minutes for time exercises level 2 & 3}
          @item{3 minutes for GAPESA level 1}
          @item{3.5 minutes for GAPESA level 2 & 3}
          @item{4 minutes for GAPESA level 4 & 5}
          @item{5 minutes for GAPESA level 6 7 & 8}
          @item{6 minutes for GAPESA level 9 & 10}
          @item{3 minutes for Perimeter/Area level 1 & 4}
          @item{4 minutes for Perimeter/Area level 2 3 & 6}
          @item{5 minutes for Perimeter/Area level 5}]

@section{Pause Button}

The button above all start exercise buttons is labeled @bold{Pause}. The purpose for this
is a toilet break. If a student needs to go to the toilet, they can press this button,
and exercises will be suspended. Most importantly the running time will be stopped, and
the input-field will not accept any inputs.
When the student is ready to resume, she/he can click again on @bold{Resume} button
(after the first click, the button label has changed from Pause to Resume), and exercises
will resume. The time spent during the break will not count.
However, any set of exercises can be paused only one time. After that the @bold{Pause} button is
disabled until the next set of exercises is started.
Pause button is only active if number of exercises is set to 5 or more.

@section{Font Adjustment}

On computers with small display, most of the default font sizes that the program is using
are to small for comfortable use. Therefore the font adjustment @bold{Preferences} menu was
added to the program. That menu contains several sub-menus for changing the size of
individual fonts, or all fonts at the same time in increments up to +3/-3 point sizes.
Whenever the font size(s) are changed with one of the Font sub-menus, the program has to
be restarted for the change to take effect. However, there's no need to update fonts every
time, as the program is (after every change of fonts) updating @bold{.jmq-fontrc} file
(in the same directory as the program).

@section{Stop Button}

To exit/stop the running set of exercises, use the @bold{Stop} button. You can then start
the next set of exercises, with the same setup you have set before with the @bold{Setup}
menu.

@section{Clear Reports}

The last sub-menu under @bold{Setup} menu is @bold{Clear all reports}. This will
erase all report printouts from previous exercises.

@section{Help menu}

@bold{Help} menu contains 4 sub-menus: @bold{Documentation}, @bold{HTML Documentation},
@bold{About Math Quiz}, and @bold{Update math-quiz}.
Of those 4, only the @bold{HTML Documentation} is enabled all the time, because when you
start it, it opens up in a browser and does not impede your interaction with the program.
You can have it opened all the time, and still continue to interact with the program.
The other 3 are disabled whenever the set of exercises is running, because they block the
rest of the program. They get enabled again, when the running set of exercises is completed,
or when the pause is in effect (@bold{Pause} button has been used). However, note that
@bold{Update math-quiz} is a special case, explained below.

@subsection{Update math-quiz}

One of the sub-menus under @bold{Help} menu is @bold{Update math-quiz}. This menu is only
active if the user has the Racket language installed on their computer, and if math-quiz
is installed via Racket Package Installer.
If this is not the case (you are running binary only version downloaded from
@bold{hrvoje064.itch.io/jasnas-math-quiz}, then the @bold{Update ...} sub-menu will be
disabled, as it serves no purpose.

@section{Exit}

When finished with exercises, exit the program by clicking @bold{X} in the right top corner of the
main window.
