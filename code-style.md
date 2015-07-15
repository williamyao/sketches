Google Common Lisp Style Guide
==============================

"Pattern means 'I have run out of language.'" -- Rich Hickey

###Background

Common Lisp is a powerful multiparadigm programming language. With great power comes great responsibility.

This guide recommends formatting and stylistic choices designed to make your code easier for other people to understand. For those internal applications and free software libraries that we develop at Google, you should keep within these guidelines when making changes. Note however that each project has its own rules and customs that complement or override these general guidelines; the speed-oriented QPX low fare search engine notably has a very different style and feel from the QRes reservation system.

If you're writing Common Lisp code outside Google, we invite you to consider these guidelines. YOu may find some of them useful where they don't conflict with other priorities you have. We welcome remarks and constructive feedback on how to improve our guide, and on what alternate styles work for you and why.

This guide is not a Common Lisp tutorial. For basic information about the language, please consult Practical Common Lisp. For a language reference, please consult the Common Lisp Hyperspec. For more detailed style guidance, take (with a pinch of salt) a look at Peter Norvig and Kent Pitman's style guide.

###Meta-Guide

####Must, Should, May, or Not

Each guideline's level of importance is indicated by use of the following keywords and phrases, adapted from RFC 2119.

+ **MUST**: This word, of the terms "REQUIRED" or "SHALL", means that the guideline is an absolute requirement. You must ask permission to violate a MUST.
+ **MUST NOT**: This phrase, or the phrase "SHALL NOT", means that the guideline is an absolute prohibition. You must ask permission to violate a MUST NOT.
+ **SHOULD**: This word, or the adjective "RECOMMENDED", means that there may exist valid reasons in particular circumstances to ignore the demands of the guideline, but the full implications must be understood and carefully weighted before choosing a different course. You must ask forgiveness for violating a SHOULD.
+ **SHOULD NOT**: This phrase, or the phrase "NOT RECOMMENDED", means that there may exist valid reasons in particular circumstances to ignore the prohibitions of the guideline, but the full implications should be understood and carefully weighted before choosing a different course. You must ask forgiveness for violating a SHOULD NOT.
+ **MAY**: This word, or the adjective "OPTIONAL", means that an item is truly optional.

####Permission and Forgiveness

There are cases where transgression of some of these rules is useful or even necessary. In some cases, you must seek permission or obtain forgiveness from the proper people.

Permission comes from the owners of your project.

Forgiveness is requested in a comment near the point of guideline violation, and is granted by your code reviewer. The original comment should be signed by you, and the reviewer should add a signed approval to the comment at review time.

####Conventions

You MUST follow conventions. They are not optional.

Some of these guidelines are motivated by universal principles of good programming. Some guidelines are motivated by technical peculiarities of Common Lisp. Some guidelines were once motivated by a technical reason, but the guideline remained after the reason subsided. Some guidelines, such as those about comments and indentation, are based purely on convention, rather than on clear technical merit. Whatever the case may be, you must still follow these guidelines, as well as other conventional guidelines that have not been formalized in this document.

You MUST follow conventions. They are important for readability. When conventions are followed by default, violations of the convention are a signal that something notable is happening and deserves attention. When conventions are systematically violated, violations of the convention are a distracting noise that needs to be ignored.

Conventional guidelines *are* indoctrination. Their purpose is to make you follow the mores of the community, so you can more effectively cooperate with existing members. It is still useful to distinguish the parts that are technically motivated from the parts that are mere conventions, so you know when best to defy conventions for good effect, and when not to fall into the pitfalls that the conventions are there to help avoid.

####Old Code

Fix old code as you go.

A lot of our code was written before these guidelines existed. You should fix violations as you encounter them in the course of your normal coding.

You must not fix violations en masse without warning other developers and coordinating with them, so as not to make the merging of large branches more difficult that it already is.

####Future Topics

There are many topics for additional standardization not covered by the current version of this document, but deferred to future version.

+ File and directory structure
+ Packages and modularity
+ Threads and locking
+ How to add configurable components
+ CLOS style: initforms, slots and accessor names, etc.
+ Recommendations on max number of slots per class
+ More concrete examples of good code:
  + exceptions
  + transactions, with retry
  + XML
  + typing
  + encapsulation/abstraction
  + class and slot names
  + etc.
+ When (not) to use conditional compilation:
  + modifying the product
  + conditional debugging/console output/etc.
  + "temporarily" commenting out blocks of code
  + etc.

###General Guidelines

####Principles

When making decisions about how to write a given piece of code, aim for the following -ilities in this priority order:

1. Usability by the customer
2. Debuggability/Testability
3. Readability/Comprehensibility
4. Extensibility/Modifiability
5. Efficiency (of the Lisp code at runtime)

Most of these are obvious.

Usability by the customer means that the system has to do what the customer requires; it has to handle the customer's transaction volumes, uptime requirements; etc.

For the Lisp efficiency point, given two options of equivalent complexity, pick the one that performs better. (This is often the same as the one that conses less, i.e. allocates less storage from the heap.)

Given two options where one is more complicated to understand than the other, pick the simpler option and revisit the decision only if profiling shows it to be a performance bottleneck.

However, avoid premature optimization. Don't add complexity to speed up something that runs rarely, since in the long run, it matters less whether such code is fast.

####Architecture

To build code that is robust and maintainable, it matters a lot how the code is divided into components, how these components communicate, how changes propagate as they evolve, and more importantly how the programmers who develop these components communicate as the components evolve.

If your work affects other groups, might be reusable across groups, adds new components (including QA or Ops), or otherwise isn't purely local, you must write it up using at least a couple of paragraphs, and get a design approval from the other parties involved before starting to write code -- or be ready to scratch what you have when they object.

If you don't know or don't care about these issues, ask someone who does.

####Using Libraries

Often, the smallest hammer is to use an existing library. Or one that doesn't exist yet. In such a case, you are encouraged to use or develop such a library, but you must take appropriate precautions.

+ You MUST NOT start a new library unless you established that none is already available that can be fixed or completed into becoming what you need. That's a rule against the NIH syndrome ("Not Invented Here"), which is particularly strong amongst Lisp hackers.
+ Whichever library, old or new, you pick, you MUST get permission to incorporate third-party code into the code base. You must discuss the use of such library in the appropriate mailing list, and have your code reviewed by people knowledgeable in the domain and/or the Lisp library ecosystem (if any). Please be ready to argue why this particular solution makes sense as compared to other available libraries.
+ Some libraries are distributed under licenses not compatible with the software you're writing, and must not be considered available for use. Be aware of these issues, or consult with people who are.

####Open-Sourcing Code

If you write a general-purpose library, or modify an existing open-source library, you are encouraged to publish the result separate from your main project and then have your project import it like any other open-source library.

Use your judgment to distinguish general-purpose versus business-specific code, and open-source the general-purpose parts, while keeping the business-specific parts a trade secret.

Open-Sourcing code has many advantages, including being able to leverage third parties for development, letting the development of features be user-directed, and keeping you honest with respect to code quality. Whatever code you write, you will have to maintain anyway, and make sure its quality is high enough to sustain use in production. There should therefore be no additional burden to Open-Sourcing, even of code that (at least initially) is not directly usable by third parties.

####Development Process

Development process is outside the scope of this document. However, developers should remember at least these bits: get reviewed, write tests, eliminate warnings, run tests, avoid mass-change.

+ All code changes must be reviewed. You should expect that your code will be reviewed by other hackers, and that you will be assigned other hackers' code to review. Part of the review criteria will be that code obeys the coding standards in this document.
+ You must write and check-in tests for new code that you write and old bugs you fix. There must be a unit test for every API function, and any previously failing case. Your work is not truly done until this activity is complete. Estimating tasks must include the time it takes to produce such tests.
+ Your code must compile without any compilation error or warning messages whatsoever. If the compiler issues warnings that should be ignored, muffle those warnings using the `UIOP:WITH-MUFFLED-COMPILER-CONDITIONS` and `UIOP:*UNINTERESTING-COMPILER-CONDITIONS*` framework (part of `UIOP`, part of `ASDF 3`), either around the entire project, or around individual files (using `ASDF`'s `:around-compile` hooks).
+ You must run the "precheckin" tests, and each component must pass its unit tests successfully before you commit any code.
+ Many people develop on branches. You must get permission to undertake mass-changes (e.g. mass reindentations) so that we can coordinate in advance, and give branch residents time to get back on the mainline.

###Formatting

####Spelling and Abbreviations

You must use correct spelling in your comments, and most importantly in your identifier.

When several correct spellings exist (including American vs. English), and there isn't a consensus amongst developers as to which one to use, you should choose the shorter spelling.

You must use only common and domain-specific abbreviations, and must be consistent with these abbreviations. You may abbreviate lexical variables of limited scope in order to avoid overly-long symbol names.

If you're not sure, consult a dictionary, Google for alternative spellings, or ask a local expert.

Here are examples of choosing the correct spelling:

+ Use "complimentary" in the sense of a meal or beverage that is not paid for by the recipient, not "complementary".
+ Use "existent" and "nonexistent", not "existant". Use "existence", not "existance".
+ Use "hierarchy", not "heirarchy".
+ Use "precede", not "preceed".
+ Use "weird", not "wierd",

Here are examples of choosing the shorter spelling:

+ Use "canceled", not "cancelled".
+ Use "queuing", not "queueing".
+ Use "signaled", not "signalled".
+ Use "traveled", not "travelled".
+ Use "aluminum", not "aluminium".
+ Use "oriented", not "orientated".
+ Use "color", not "colour".
+ Use "behavior", not "behaviour".

Make appropriate exceptions for industry standard nomenclature/jargon, including plain misspellings. For instance:

+ Use "referer", not "referrer", in the context of the HTTP protocol.

####Line Length

You should format source code so that no line is longer than 100 characters.

Some line length restriction is better than none at all. While old text terminals used to make 80 columns the standard, these days, allowing 100 columns seems better, since good style encourages the use of descriptive variables and function names.

####Indentation

Indent your code the way a properly configured GNU Emacs does.

Maintain a consistent indentation style throughout a project.

Indent carefully to make the code easier to understand.

Common Lisp indentation in Emacs is provided by the cl-indent library. The latest version of cl-indent is packaged with SLIME (under contrib/slime-cl-indent.el). After installing SLIME, set up Emacs to load SLIME automatically using [these instructions](http://www.common-lisp.net/project/slime/doc/html/Loading-Contribs.html), adding slime-indentation to the list of contrib libraries to be loaded in the call to slime-setup.

Ideally, use the default indentation settings provided by slime-indentation. If necessary, customize indentation parameters to maintain a consistent indentation style throughout an existing project. Parameters can be customized using the :variables setting in define-common-lisp-style.  Indentation of specific forms can be customized using the :indentation setting of define-common-lisp-style. This is particularly useful when creating forms that behave like macros or special operators that are indented differently from standard function calls (e.g. defun, labels, or let). Add a hook to 'lisp-mode-hook that calls common-lisp-set-style to set the appropriate style automatically.

Use indentation to make complex function applications easier to read. When an application does not fit on one line or the function takes many arguments, consider inserting newlines between the arguments so that each one is on a separate line. However, do not insert newlines in a way that makes it hard to tell how many arguments the function takes or where an argument form starts and ends.

```
;; Bad
(do-something first-argument second-argument (lambda (x)
    (frob x)) fourth-argument last-argument)
```

```
;; Better
(do-something first-argument
              second-argument
	      #'(lambda (x) (frob x))
	      fourth-argument
	      last-argument)
```

####File Header

You should include a description at the top of each source file.

You should include neither authorship nor copyright information in a source file.

Every source file should begin with a brief description of the contents of that file.

After that description, every file should start the code itself with an `(in-package :package-name)` form.

After than `in-package` form, every file should follow with any file-specific `(declaim (optimize ...))` declaration that is not covered by an `ASDF` `:around-compile` hook.

```
;;;; Varaiable length encoding for integers and floating point numbers.

(in-package #:varint)
(declaim #.*optimize-defaults*)
```

You should not include authorship information at the top of a file: better information is available from version control, and such a mention will only cause confusion and grief. Indeed, whoever was the main author at the time such a mention was included might not be who eventually made the most significant contributions to the file, and even less who is responsible for the file at the moment.

You should not include copyright information in individual source code files. An exception is made for files meant to be disseminataed as standalone.

####Vertical White Space

Vertical white space: one blank line between top-level forms.

You should include one blank line between top-level forms, such as function definitions. Exceptionally, blank lines can be omitted betwen simple, closely related defining forms of the same kind, such as a group of related type declarations or constant definition.

```
(defconstant +mix32+ #x12b9b0a1 "pi, an arbitrary number")
(defconstant +mix64+ #x2b992ddfa23249d6 "more digits of pi")

(defconstant +golden-ratio32+ #x9e3779b9 "the golden ratio")
(defconstant +golden-ratio64+ #xe08c1d668b756f82 "more digits of the golden ratio")

(defmacro incf32 (x y)
  "Like INCF, but for integers module 2**32"
  `(setf ,x (logand (+ ,x ,y) #xffffffff)))
(defmacro incf64 (x y)
  "Link INCF, but for integers modulo 2**64"
  `(setf ,x (logand (+ ,x ,y) #xffffffffffffffff)))
```

Blank lines can be used to separate parts of a complicated function. Generally, however, you should break a large function into smaller ones instead of trying to make it more readable by adding vertical space. If you can't you should document with a ;; comment what each of the separated parts of the function does.

You should strive to keep top-level forms, including comments but excluding the documentation string, of appropriate length; preferrably short. Forms extending beyond a single page should be rare and their use should be justified. This applies to each of the forms in an `eval-when`, rather than to the `eval-when` itself. Additionally, `defpackage` forms may be longer, since they may include long lists of symbols.

####Horizontal White Space

Horizontal white space: none around parentheses. No tabs.

You must not include extra horizontal whitespace before or after parentheses or around symbols.

You must not place right parentheses by themselves on a line. A set of consecutive trailing parentheses must appear on the same line.

```
;; Very Bad
( defun factorial ( limit )
  ( let (( product i ))
    ( loop for i from i upto limit
          do (setf product ( * product i ) ) )
    product
  )
)
```

```
;; Better
(defun factorial (limit)
  (let ((product i))
    (loop for i from 1 upto limit
          do (setf product (* product i)))
    product))
```

You should only use one space between forms.

You should not use spaces to vertically align forms in the middle of consecutive lines. An exception is made when the code possesses an important yet otherwise not visible symmetry that you want to emphasize.

```
;; Bad
(let* ((low    1)
       (high   2)
       (sum    (+ (* low low) (* high high))))
  ...)
```

```
;; Better
(let* ((low 1)
       (high 2)
       (sum (+ (* low low) (* high high))))
  ...)
```

You must align nested forms if they occur across more than one line.

```
;; Bad
(defun munge (a b c)
(* (+ a b)
c))
```

```
;; Better
(defun munge (a b c)
  (* (+ a b)
     c))
```

The contention is that the body of a binding form is indented two spaces after the form. Any binding data before the body is usually indented four spaces. Arguments to a function call are aligned with the first argument; if the first argument is on its own line, it is aligned with the function name.

```
(multiple-value-bind (a b c d)
    (function-returning-four-values x y)
  (declare (ignore c))
  (something-using a)
  (also-using b d))
```

An exception to the rule against lonely parentheses is made for an `eval-when` form around several definitions; in this case, include a `; eval-when` after the closing parenthesis.

You must set your editor to avoid inserting tab characters in the files you edit. Tabs cause confusion when editors disagree on how many spaces they represent. In Emacs, do `(setq-default indent-tabs-mode nil)`.

###Documentation

####Document Everything

You should use document strings on all visible functions to explain how to use your code.

Unless some bit of code is painfully self-explanatory, document it with a documentation string (also known as a docstring).

Documentation strings are destined to be read by the programmers who use your code. They can be extracted from functions, types, classes, variables and macros, and displayed by programming tools, such as IDEs, or by REPL queries such as `(describe 'foo)`; web-based documentation or other reference works can be created based on them. Documentation strings are thus the perfect locus to document your API. They should describe how to use the code (including what pitfalls to avoid), as opposed to how the code works (and where more work is needed), which is what you'll put in comments.

Supply a documentation string when defining top-level functions, types, classes, variables and macros. Generally, add a documentation string wherever the language allows.

For functions, the docstring should describe the function's contract: what the function does, what the arguments mean, what values are returned, what conditions the function can signal. It should be expressed at the appropriate level of abstraction, explaining the intended meaning rather than, say, just the syntax. In documentation strings, capitalize the names of Lisp symbols, such as function arguments. For example, "The value of LENGTH should be an integer."

```
(defun small-prime-number-p (n)
  "Return T if N, an integer, is a prime number. Otherwise, return NIL."
  (cond ((or (< n 2))
         nil)
        ((= n 2)
	 t)
	(t
	 (loop for i from 3 upto (sqrt n) by 2
	       never (divisorp i n)))))

(defgeneric table-clear (table)
  (:documentation
    "Like clrhash, empties the TABLE of all
    associations, and returns the table itself."))
```

A long docstring may usefully begin with a short, single-sentence summary, followed by the larger body of the docstring.

When the name of a type is used, the symbol may be quoted by surrounding it with a back quote at the beginning and a single quote at the end. Emacs will highlight the type, and the highlighting serves as a cue to the reader that M-. will lead to the symbol's definition.

```
(defun bag-tag-expected-itinerary (bag-tag)
  "Return a list of `legacy-pnr-pax-segment' objects representing
  the expected itinerary of the `bag-tag' object, BAG-TAG."
  ...)
```

####Comment Semicolons

You must use the appropriate number of semicolons to introduce comments.

Comments are explanations to the future maintainers of the code. Even if you're the only person who will ever see and touch the code, even if you're either immortal and never going to quit, or unconcerned with what happens after you leave (and have your code self-destruct in such an eventuality), you may find it useful to comment your code. Indeed, by the time you revisit your code, weeks, months or years leater, you will find yourself a different person from the one who wrote it, and you will be grateful to that previous self for making the code readable.

You must comment anything complicated so the next developer can understand what's going on. (Again, the "hit by a truck" principle.)

Also use comments as a way to guide those who read the code, so they know what to find where.

+ File headers and important comments that apply to large sections of code in a source file should begin with four semicolons.
+ You should use three semicolons to being comments that apply to just one top-level form or small group of top-level forms.
+ Inside a top-level form, you should use two semicolons to begin a comment if it appears between lines.
+ You should use one semicolon if it is a parenthetical remark and occurs at the end of a line. You should use spaces to separate the comment from the code it refers to so the comment stands out. You should try to vertically align consecutive related end-of-line comments.

```
;;;; project-euler.lisp
;;;; File-level comments or comments for large sections of code.

;;; Problems are described in more detail here:  http://projecteuler.net/

;;; Divisibility
;;; Comments that describe a group of definitions.

(defun divisorp (d n)
  (zerop (mod n d)))

(defun proper-divisors (n)
  ...)

(defun divisors (n)
  (cons n (proper-divisors n)))

;;; Prime numbers

(defun small-prime-number-p (n)
  "Return T if N, an integer, is a prime number. Otherwise, return NIL."
  (cond ((or (< n 2))
         nil)
        ((= n 2)   ; parenthetical remark here
	 t)        ; continuation of the remark
	((divisorp 2 n)
	 nil)  ; different remark
	;; Comment that applies to a section of code
	(t
	 (loop for i from 3 upto (sqrt n) by 2
	       never (divisorp i n)))))
```

You should include a space between the semicolon and the text of the comment.

####Grammar and Punctuation

You should punctuate documentation correctly.

When a comment is a full sentence, you should capitalize the initial letter of the first word and end the comment with a period. In general, you should use correct punctuation.

####Attention Required

You must follow the convention of using TODO comments for code requiring special attention. For code using unobvious forms, you must include a comment.

For comments requiring special attention, such as incomplete code, todo items, questions, breakage, and danger, include a TODO comment indicating the type of problem, its nature, and any notes on how it may be addressed.

The comments begin with a `TODO` in all capital letters, followed by the name, e-mail address, or other identifier of the person with the best context about the problem referenced by the `TODO`. The main purpose is to have a consistent `TODO` that can be searched to find out how to get more details upon request. A `TODO` is not a commitment that the person referenced will fix the problem. Thus when you create a `TODO`, it is almost always your name that is given.

When signing comments, you should use your username (for code within the company) or full email address (for code visible outside the company), not just initials.

```
;;--- TODO(george@gmail.com): Refactor to provide a better API.

Be spsecific when indicating times or software released in a TODO comment and use YYYY-MM-DD format for dates to make automated processing of such dates easier, e.g., 2038-01-20 for the end of the 32-bit signed `time_t`.

```
;;---(brown): Remove this code after release 1.7 or before 2012-11-30.
```

For code that uses unobvious forms to accomplish a task, you must include a comment stating the purpose of the form and the task it accomplishes.

####Domain-Specific Languages

You should document DLSs and any terse program written in a DSL.

You must design your Domain Specific Language to be easy to read and understand by people familiar with the domain.

You must properly document all your Domain Specific Languages.

Sometimes, your DSL is designed for terseness. In that case, it is important to document what each program does, it it's not painfully obvious from the context.

Notably, when you use regular expressions (e.g. with the `CL-PPCRE` package), you MUST ALWAYS put in a comment (usually a two-semicolon comment on the previous line) explaining, at least basically, what the regular expression does, or what the purpose of using it is. The comment need not spell out every bit of the syntax, but it should be possible for someone to follow the logic of the code without actually parsing the regular expression.

###Naming

####Symbol Guidelines

You should use lower case. You should follow the rules for Spelling and Abbreviations. You should follow punctuation conventions.

Use lower case for all symbols. Consistently using lower case makes searching for symbol names easier and is more readable.

Note that Common Lisp is case-converting, and that the `symbol-name` of your symbols will be upper case. Because of this case-converting, attempts to distinguish symbol by case are defeated, and only result in confusion. While it is possible to escape characters in symbols to force lowercase, you should not use this capability unless this is somehow necessary to interoperate with third-party software.

Place hyphens between all the words in a symbol. If you can't easily say an identifier out loud, it is probably badly named.

You must not use "/" or "." instead of "-" unless you have a well-documented overarching reason to, and permission from other hackers who review your proposal.

See the section on Spelling and Abbreviations for guidelines on using abbreviations.

```
;; Bad
(defvar *default-username* "Ann")
(defvar *max-widget-cnt* 200)
```

```
;; Better
(defvar *default-user-name* "Ann")
(defvar *maximum-widget-count* 200)
```

There are conventions in Common Lisp for the use of punctuation in symbols. You should not use punctuation in symbols outside these conventions.

Unless the scope of the variable is very small, do not use overly short names like `i` and `zq`.

####Denote Intent, not Content

Name your variables according to their intent, not their content.

You should name a variable according to the high-level concept that it represents, not according to the low-level implementatin details of how the concept is represented.

Thus, you should avoid embedding data structure or aggregate type names, such as `list`, `array`, or `hash-table` inside variable names, unless you're writing a generic algorithm that applies to arbitrary lists, arrays, hash-tables, etc. In that case it's perfectly OK to name a variable `list` or `array`.

Indeed, you should be introducing new abstract data types with `DEFCLASS` or `DEFTYPE`, whenever a new kind of intent appears for objects in your protocols. Functions that manipulate such objects generically may then use variables the name of which reflect that abstract type.

For example, if a variable's value is always a row (or is either a row of `NIL`), it's good to call it `row` or `first-row` or something like that. It is alright if `row` has been `DEFTYPED` to `STRING` -- precisely because you have abstracted the detail away, and the remaining salient point is that it is a row. You should not name the variable `STRING` in this context, except possibly in low-level functions that specifically manipulate the innards of rows to provide the suitable abstraction.

Be consistent. If a variable is named `row` in one function, and its value is being passed to another function, call it `row` rather than, say, `value` (this was a real case).

####Global Variables and Constants

Name globals according to convention.

The names of global constants should start and end with plus characters.

Global variable names should start and end with asterisks (also known in this context as earmuffs).

In some projects, parameters that are not meant to be usually modified or bound under normal circumstances (but may be during experimentation or exceptional situations) should start (but do not end) with a dollar sign. If such a convention exists within your project, you should follow it consistently. Otherwise, you should avoid naming variables like this.

Common Lisp does not have global lexical variables, so a naming convention is used to ensure that globals, which are dynamically bound, never have names that overlap with local variables. It is possible to fake global lexical variables with a differently named global variable and a `DEFINE-SYMBOL-MACRO`. You should not use this trick, unless you first publish a library that abstracts it away.

```
(defconstant +hash-results+ #xbd49d10d10cbee50)

(defvar *maximum-search-depth* 100)
```

####Predicate Names

Names of predicate functions and variables end with a `"P"`.

You should name boolean-valued functions and variables with a trailing `"P"` or `"-P"`, to indicate they are predicates. Generally, you should use `"P"` when the rest of the function name is one word and `"-P"` when it is more than one word.

A rationale for this convention is given in the CLtL2 chapter on predicates.

For uniformity, you should follow the convention above, and not one of the alternatives below.

An alternative rule used in some existing packages is to always use `"-P"`. Another alternative rule used in some existing packages is to always use `"?"`. When you develop such a package, you must be consistent with the rest of the package. When you start a new package, you should not use such an alternative rule without a very good documented reason.

####Omit Library Prefixes

You should not include a library or package name as a prefix within the name of symbols.

When naming a symbol (external or internal) in a package, you should not include the package name within the name of the symbol. Naming a symbol this way makes it awkward to use from a client package accessing the symbol by qualifying it with a package prefix, where the package name then appears twice (once as a package prefix, another time as a prefix within the symbol name).

```
;; Bad
(in-package #:varint)
(defun varint-lenfth64 () ... )

(in-package #:client-code)
(defconst +padding+ (varint:varint-length64 +end-token+))
```

```
;; Better
(in-package #:varint)
(defun length64 () ... )

(in-package #:client-code)
(defconst +padding+ (varint:length64 +end-token+))
```

An exception to the above rule would be to include a prefix for the names of variables that would otherwise be expected to clash with variables in packages that use the current one. For instance, `ASDF` exports a variable `*ASDF-VERBOSE*` that controls the verbosity of `ASDF` only, rather than of the entire Lisp program.

####Packages

Use packages appropriately.

Lisp packages are used to demarcate namespaces. Usually, each system has its own namespace. A package has a set of external symbols, which are intended to be used from outside the package, in order to allow other modules to use this module's facilities.

The internal symbols of a package should never be referred to from other packages. That is, you should never have to use the double-colon `::` construct, (e.g. `QUAKE::HIDDEN-FUNCTION`). If you need to use double-colons to write real production code, something is wrong and needs to be fixed.

As an exception, unit tests may use the internals of the package being tested. So when you refactor, watch out for internals used by the package's unit tests.

The `::` construct is also useful for very temporary hacks, and at the REPL. But if the symbol really is part of the externally-visible definition of the package, export it.

You may find that some internal symbols represent concepts you usually want to abstract away and hide under the hood, yet at times are necessary to expose for various extensions. For the former reason, you do not want to export them, yet for the latter reason, you have to export them. The solution is to have two different packages, one for your normal users to use, and another for the implementation and its extenders to use.

Each package is one of two types:

* Intended to be included in the `:use` specification of other packages. If package `A` "uses" package `B`, then the external symbols of package `B` can be referenced from within package `A` without a package prefix. We mainly use this for low-level modules that provide widely-used facilities.
* Not intended to use "used". To reference a facility provided by package `B`, code in package `A` must use an explicit package prefix, e.g. `B:DO-THIS`.

If you add a new package, it should always be of the second type, unless you have a special reason and get permission. Usually a package is designed to be one or the other, by virtue of the names of the functions. For example, if you have an abstraction called `FIFO`, and it were in a package of the first type you'd have functions named things like `FIFO-ADD-TO` and `FIFO-CLEAR-ALL`. (`FIFO:FIFO-CLEAR-ALL is redundant and ugly.)

Another good thing about packages is that your symbol names won't collide with the names of other packages, except the ones your packages "use". So you have to stay away from symbols that are part of the Lisp implementation (since you always "use" that) and that are part of any other packages you "use", but otherwise you are free to make up your own names, even short ones, and not worry about someone else having used the same name. You're isolated from each other.

Your package must not shadow (and thus effectively redefine) symbols that are part of the Common Lisp language. There are certain exceptions, but they should be very well-justified and extremely rare:

* If you are explicitly replacing a Common Lisp symbol by a safer or more featureful version.
* If you are defining a package not meant to be "used", and have a good reason to export a symbol that clashes with Common Lisp, such as `log:error`, `log:warn` and so on.

###Language Usage Guidelines

####Mostly Functional Style

You should avoid side-effects when they are not necessary.

Lisp is best used as a "mostly functional" language.

Avoid modifying local variables, try rebinding instead.

Avoid creating objects and then SETFing their slots. It's better to set the slots during initialization.

Make classes as immutable as possible, that is, avoid giving slots setter functions if at all possible.

Using a mostly functional style makes it much easier to write concurrent code that is thread-safe. It also makes it easier to test the code.

####Recursion

You should favor iteration over recursion.

Common Lisp systems are not required to implement function calls from tail positions without leaking stack space -- which is known as proper tail calls (PTC), tail call elimination (TCE), or tail call optimization (TCO). This means that indefinite recursion through tail calls may quickly blow out the stack, which hampers functional programming. Still, most serious implementations (including SBCL and CCL) do implement proper tail calls, but with restrictions:

* The `(DECLARE (OPTIMIZE ...))` settings must favor `SPEED` enough and not favor `DEBUG` too much, for some compiler-dependent meanings of "enough" and "too much". (For instance, in SBCL, you should avoid `(SPEED 0)` and `(DEBUG 3)` to achieve proper tail calls.)
* There should not be dynamic bindings around the call (even though some Scheme compilers are able to properly treat such dynamic bindings, called parameters in Scheme parlance.)

For compatability with all compilers and optimization settings, and to avoid stack overflow when debugging, you should prefer iteration or the built in mapping functions to relying on proper tail cails.

If you do rely on proper tail calls, you must prominently document the fact, and take appropriate measures to ensure and appropriate compiler is used with appropriate optimization settings. For fully portable code, you may have to use trampolines instead.

####Special Variables

Use special variables sparingly.

Using Lisp "special" (dynamically bound) variables as implicit arguments to functions should be done sparingly, and only in cases where it won't surprise the person reading code, and where it offers significant benefits.

Indeed, each special variable constitutes state. Developers have to mentally track the state of all relevant variables when trying to understand what the code does and how it does it; tests have to written and run with all the relevant combinations; to isolate some activity, care has to be taken to locally bind all relevant variables, including those of indirectly used modules. They can hide precious information from being printed in a backtrace. Not only is there overhead associated with each new variable, but interactions between variables can make the code exponentially more complex as the number of such variables increases. The benefits have to match the costs.

Note though that a Lisp special variable is not a global variable in the sense of a global variable in, say, BASIC or C. As special variables can be dynamically bound to a local value, they are much more powerful than global value cells where all users necessarily interfere with each other.

Good candidates for such special variables are items for which "the current" can be naturally used a prefix, such as "the current database connection" or "the current business data source". They are singletons as far as the rest of the code is concerned, and often passing them as an explicit argument does not add anything to the readability or maintainability of the source code in question.

They can make is easier to write code that can be refactor. If you have a request processing chain, with a number of layers that all operate upon a "current" request, passing the request object explicitly to every function requires that every function in the chain have a request argument. Factoring out code into new functions often requires that these functions also have this argument, which clutters the code with boilerplate.

You should treat special variables as though they are per-thread variables. By default, you should leave a special variable with no top-level binding at all, and each thread of control that needs the variable should bind it explicitly. This will mean that any incorrect use of the variable will result in an "unbound variable" error, and each thread will see its own value for the variable. Variables with a default global value should usually be locally bound at thread creation time. You should use suitable infrastructure to automate the appropriate declaration of such variables.

####Assignment

Be consistent in assignment forms.

There are several styles for dealing with assignment and side-effects; whichever a given package is using, keep using the same consistently when hacking said package. Pick a style that makes sense when starting a new package.

Regarding multiple assignment in the same form, there are two schools: the first style groups as many assignments as possible into a single `SETF` or `PSETF` form thus minimizing the number of forms with side-effects; the second style splits assignment into as many individual `SETF` (or `SETQ`, see below) forms as possible, to maximize the chances of located forms that modify a kind of place by grepping for `(setf (foo ...` A grep pattern must actually contain as many place place-modifying forms as you may use in your programs, which may make this rationale either convincing or moot depending on the rest of the style of your code. You should follow the conventions used in the package you are hacking. We recommend the first convention for new packages.

Regarding `SETF` and `SETQ`, there are two schools: the first regards `SETQ` as an archaiv implementation detail, and avoids it entirely in favor of `SETF`; the second regards `SETF` as an adiitional layer of complexity, and avoids it in favor of `SETQ` whenever possible (i.e. whenever the assigned place is a variable or symbol-macro). You should follow the convention used in the package you are hacking. We recommend the first convention for new packages.

In the spirit of a mostly pure functional style, which makes testing and maintenance easier, we invite you to consider how to do things with the fewest assignments possible.

####Assertions and Conditions

You must make proper usage of assertions and conditions.

* `ASSERT` should be used ONLY to detect internal bugs. Code should `ASSERT` invariants whose failure indicates that the software is itself broken. Incorrect input should be handled properly at runtime, and must not cause an assertion violation. The audience for an `ASSERT` failure is a developer. Do not use the data-form and argument-form in `ASSERT` to specify a condition to signal. It's fine to use them to print out a message for debugging purposes (and since it's only for debugging, there's no issue of internationalization.)
* `CHECK-TYPE`, `ETYPECASE` are also forms of assertions. When one of these fails, that's a detected bug. You should prefer to use `CHECK-TYPE` over `(DECLARE (TYPE ...))` for the inputs of functions.
* Your code should use assertions and type checks liberally. The sooner a bug is discovered, the better! Only code in the critical path for performance and internal helpers should eschew explicit assertions and type checks.
* Invalid input, such as files that are read but do not conform to the expected format, should not be treated as assertion violations. Always check to make sure that input is valid, and take appropriate actions if it is not, such as signalling a real error.
* `ERROR` should be used to detect problems with user data, requests, permissions, etc., or to report "unusual outcomes" to the caller.
* `ERROR` should always be called with an explicit condition type; it should never simply be called with a string. This enables internationalization.
* Functions that report unusual outcomes by signaling a condition should say so explicitly in their contracts (their textual descriptions, in documentation and docstrings etc.). When a function signals a condition that is not specified by its contract, that's a bug. The contract should specify the condition class(es) clearly. The function may then signal any condition that is a type-of any of those conditions. That is, signaling instances of subclasses of the documented condition classes is fine.
* Complex bug-checks may need to use `ERROR` instead of `ASSERT`.
* When writing a server, you must not call `WARN`. Instead, you should use the appropriate logging framework.
* Code must not call `SIGNAL`. Instead, use `ERROR` or `ASSERT`.
* Code should not use `THROW` and `CATCH`; instead use the `RESTART` facility.
* Code should not generically handle all conditions, e.g. type `T`, or use `IGNORE-ERRORS`. Instead, let unknown conditions propagate to the standard ultimate handler for processing.
* There are a few places where handling all conditions is appropriate, but they are rare. The problem is that handling all conditions can mask program bugs. If you *do* need to handle "all conditions", you MUST handle only `ERROR`, *not* `T` and not `SERIOUS-CONDITION`. (This is notably because CCL's process shutdown depends on being able to signal `process-reset` and have it handled by CCL's handler, so we must not interpose our own handler.
* `(error (make-condition 'foo-error ...))` is equivalent to `(error 'foo-error ...) -- code must use the shorter form.
* Code should not signal conditions from inside the cleanup form of `UNWIND-PROTECT` (unless they are alwys handled inside the cleanup form), or otherwise do non-local exits from cleanup handlers outside of the handler e.g. `INVOKE-RESTART`.
* Do not clean up by resignaling. If you do that, and the condition is not handled, the stack trace will halt at the point of the resignal. And the rest is the part we really care about!

```
;; Bad
(handler-case
  (catch 'ticket-at
    (etd-process-blocks))
  (error (c)
    (reset-parser-values)
    (error c)))
```

```
;; Better
(unwind-protect
  (catch 'ticket-at
    (etd-process-blocks))
  (reset-parser-values))
```

####Type Checking

If you know the type of something, you should make it explicit in order to enable compile-time and run-time sanity-checking.

If your function is using a special variable as an implicit argument, it's good to put in a `CHECK-TYPE` for the special variable, for two reasons: to clue in the person readin the code that this variable is being used implicitly as an argument, and also to help detect bugs.

Using `(declare (type ...))` is the least desirable mechanism to use because, as Scott McKay puts it:

> The fact is, `(declare (type ...))` does different things depending on the compiler settings of speed, safety, etc. In some compilers, when speed is greater than safety, `(declare (type ...))` will tell the compiler "please assume that these variables have these types" *without* generating any type-checks. That is, if some variable has the value `1432` in it, and you declare it to be of type `string`, the compiler might just go ahead and use it as though it's a string.

> Moral: don't use `(declare (type ...))` to declare the contract of any API functions, it's not the right thing. Sure, use it for "helper" functions, but not API functions.

You should, of course, use appropriate declarations in internal low-level functions where these declarations are used for optimization. When you do, however, see our recommendations for Unsafe Operations.

####CLOS

Use CLOS appropriately.

When a generic function is intended to be called from other modules (other parts of the code), there should be an explicit `DEFGENERIC` form, with a `:DOCUMENTATION` string explaining the generic contract of the function (as opposed to its behavior for some specific class). It's generally good to do explicit DEFGENERIC forms, but  for module entry points it is mandatory.

When the argument list of a generic function includes `&KEY`, `DEFGENERIC` should always explicitly list all of the keyword arguments that are acceptable, and explain what they mean. (Common Lisp does not require this, but it is good form, and it may avoid spurious warnings on SBCL.)

You should avoid `SLOT-VALUE` and `WITH-SLOTS`, unless you absolutely intend to circumvent any sort of method combination that might be in effect for the slot. Rare exceptions include `INITIALIZE-INSTANCE` and `PRINT-OBJECT` methods and accessing normally hidden slots in the low-level implementation of methods that provide user-visible abstractions. Otherwise, you should use accessors, `WITH-ACCESSORS`.

Accessor names generally follow a convention of `<protocol-name>-<slot-name>`, where a "protocol" in this case loosely indicates a set of functions with well-defined behavior.

No implication of a formal "protocol" concept is necessarily intended, much less first-class "protocol" objects. However, there may indded be an abstract CLOS class or an Interface-Passing Style interface that embodies the protocol. Further (sub)classes or (sub)interfaces may then implement all or part of a protocol by defining some methods for (generic) functions in the protocol, including readers and writers.

For example, if there were a notational protocol called `pnr` with accessors `pnr-segments` and `pnr-passengers`, then the classes `air-pnr`, `hotel-pnr` and `car-pnr` could each reasonably implement methods for `pnr-segments` and `pnr-passengers` as accessors.

By default, an abstract base class name is used as the notional protocol name, so accessor names default to `<class-name>-<slot-name>`; while such names are thus quite prevalent, this form is neither required nor even preferred. In general, it contributes to "symbol bloat", and in many cases has led to a proliferation of "trampoline" methods.

Accessors named `<slot-name>-of` should not be used.

Explicit `DEFGENERIC` forms should be used when there are (or it is anticipated that there will be) more than one `DEFMETHOD` for that generic function. The reason is that the documentation for the generic function explains the abstract conttract for the function, as opposed to explaining what an individual method does for some specific class(es).

You must not use generic functions where there is no notional protocol. To put it more concretely, if you have more than one generic function that specializes its Nth argument, the specializing classes should all be descendants of a single class. Generic functions must not be used for "overloading", i.e. simply to use the same name for two entirely unrelated types.

More precisely, it's not really whether they descend from a common superclass, but whether they obey the same "protocol". That is, the two classes should handle the same set of generic functions, as if there were an explicit `DEFGENERIC` for each method.

Here's another way to put it. Suppose you have two classes, A and B, and a generic function F. There are two methods for F, which dispatch on an argument being of types A and B. Is it plausible that there might be a function call somewhere in the program that calls F, in which the argument might sometimes, at runtime, be of class A and other times be of class B? If not, you are probably overloading and should not be using a single generic function.

We allow one exception to this rule: it's OK to do overloading if the corresponding argument "means" the same thing. Typically one overloading allows an X object, and the other allows the name of an X object, which might be a symbol or something.

You must not use MOP "intercessory" operations at runtime. You should not use MOP "intercessory" operations at compile-time. At runtime, there are at worst a danger, at best a performance issue. At compiler-time, it is usually cleaner that macros should set things up the right way in one pass than have to require a second pass of fixups through intercessin; but sometimes, fixups are necessary to resolve forward references, and intercession is allowed then. MOP intercession is a great tool for interactive development, and you may enjoy it while developing and debugging, but you should not use it in normal applications.

If a class definition creates a method as a `:READER`, `:WRITER`, or `:ACCESSOR`, do not redefine that method. It's OK to add `:BEFORE`, `:AFTER`, and `:AROUND` methods, but don't override the primary method.

In methods with keyword arguments, you must always use `&KEY`, even if the method does not care about the values of any keys, and you should never use `&ALLOW-OTHER-KEYS`. As long as a keyword is accepted by any method of a generic function, it's OK to use it in the generic function, even if the other methods of the same generic function don't mention it explicitly. This is particularly important for `INITIALIZE-INSTANCE` methods, since if you did uses `&ALLOW-OTHER-KEYS`, it would disable error checking for misspelled or wrong keywords in `MAKE-INSTANCE` calls!

A typical `PRINT-OBJECT` method might look like this:

```
(defmethod print-object ((p person) stream)
  (print-unreadable-object (p stream :type t :identity t)
    (with-slots (first-name last-name) p
      (format stream "~a ~a" first-name last-name))))
```

###Meta-language Guidelines

####Macros

Use macros as appropriate, which is often. Define macros when appropriate, which is seldom.

Macros bring syntactic abstraction, which is a wonderful thing. It helps make your code clearer, by describing your intent without getting bodded in implementation details (indeed abstracting those details away). It helps make your code more concise and more readable, by eliminating both redundancy and irrelevant details. But it comes at a cost to the reader, which is learning a new syntactic concept for each macro. And so it should not be abused.

The general conclusion is that there shouldn't be any recognizable *design pattern* is a good Common Lisp program. The one and only pattern is *use the language*, which includes defining and using syntactic abstractions.

Existing macros must be used whenever they make  code clearer by converying intent in a more concise way, which is often. When a macro is available in your project that expresses the concept you're using, you must not write the expansion rather than use the macro.

New macros should be defined as appropriate, which should be seldom, for common macros have already been provided by the language and its various libraries, and your program typically only needs few new ones relative to its size.

You should follow the OAOOM rule of thumb for deciding when to create a new abstraction, whether syntactic or not: if a particular pattern is used more than twice, it should probably be abstracted away. A more refined rule to decide when to use abstraction should take into account the benefit in terms of number of uses and gain at each use, to the costs in terms of having to get used to reading the code. For syntactic abstractions, costs and benefits to the reader is usually more important that costs and benefits to the writer, because good code is usually written once and read many times by many people (including the same programmer who has to maintain the program after having forgotten it). Yet the cost to the writer of the macro should also be taken into account; however, in doing so it should rather be compared to the cost of the programmer writing other code instead that may have higher benefits.

Using Lisp macros properly requires taste. Avoid writing complicated macros unless the benefit clearly outweights the cost. It takes more effort for your fellow developers to learn your macro, so you should only use a macro if the gain in expressiveness is big enough to justify that cost. As usual, feel free to consult your colleagues if you're not sure, since without a lot of Lisp experience, it can be hard to make this judgment.

You must never use a macro where a function will do. That is, if the semanctics of what you are writing conforms to the semantics of a function, then you must write it as a function rather than a macro.

You must not transform a function into a macro for performance reaons. If profiling shows that you have a performance problem with a specific function `FOO`, document the need and profiling-results appropriately, and `(declaim (inline foo))`.

You can also use a compiler-macro as a way to speed up function execution by specifying a source-to-source transformation. Beware that it interferes with tracing the optimized function.

When you write a macro-defining macro (a macro that generates macros), document and comment it particularly clearly, since these are harder to understand.

You must not install new reader macros without a consensus among the developers of your system. Reader macros must not leak out of the system that uses them to clients of that system or other systems used in the same project. You must use software such as `cl-readsyntax` or `named-readtables` to control how reader macros are used. Then clients who desire to may use the same reader macros as you do. In any case, your system must be usable even to clients who do not use these reader macros.

If your macro has a parameter that is a lisp form that will be evaluated when the expanded code is run, you should name the parameter with the suffix `-form`. This convention helps make it clearer to the macro's user which parameters are Lisp forms to be evaluated, and which are not. The common names `body` and `end` are exceptions to this rule.

You should follow the so-called `CALL-WITH` style when it applies. This style is explained at length in http://random-state.net/log/3390120648.html. The general principle is that the macro is strictly limited to processing the syntax, and as much of the semantics as possible is kept in normal functions. Therefore, a macro `WITH-FOO` is often limited to generating a call to an auxiliary function `CALL-WITH-FOO` with arguments deduced from the macro arguments. Macaro `&body` arguments are typically wrapped into a lambda expression of which they become the body, which is passed as one of the arguments of the auxiliary function.

The separation of syntactic and semantic concerns is a general principle of style that applies beyond the case of `WITH-` macros. Its advantages are many. By keeping semantics outside the macro, the macro is made simpler, easier to get right, and less subject to change, which makes it easier to develop and maintain. The semantics is written in a simpler language -- one with staging -- which also makes it easier to develop and maintain. It becomes possible to debug and update the semantic function without having to recompile all clients of the macro. The semantic function appears in the stack trace which also helps debug client functions. The macro expansion is made shorter and each expansion shares more code with other expansions, which reduces memory pressure which in turn usually makes things faster. It also makes sense to write the semantic functions first, and write the macros last as syntactic sugar on top. You should use this style unless the macro is used in tight loops where performance matters, and even then, see our rules regarding optimization.

Any functions (closures) created by the macro should be named, which can be done using `FLET`. This also allows you toe declare the function to be of dynamic extent (if it is -- and often it is; yet see below regarding DYNAMIC-EXTENT).

If a macro call contains a form, and the macro expansion includes more than one copy of that form, the form can be evaluated more than once, and code it contains macro-expanded and compiled more than once. If someone uses the macro and calls it with a form that has side effects or that takes a long time to compute, the behavior will be undesirable (unless you're intentionally writing a control structure such as a loop). A convenient way to avoid this problem is to evaluate the form only once, and bind a (generated) variable to the result. There is a very useful macro called `ALEXANDRIA:ONCE-ONLY` that generates code to do this. See also `ALEXANDRIA:WITH-GENSYMS`, to make some temporary variables in the generated code. Note that if you follow our `CALL-WITH` style, you typically expand the code only once, as either an argument to the auxiliary function, or the body of a lambda passed to it; you therefore avoid the above complexity.

When you write a macro with a body, such as `WITH-XXX` macro, even if there aren't any parameters, you should leave space for them anyway. For example, if you invent `WITH-LIGHTS-ON`, do not make the call to it look like `(defmacro with-lights-on (&body b) ...)`. Instead, do `(defmacro with-lights-on (() &body) ...)`. That way, if parameters are needed in the future, you can add them without necessarily having to change all the uses of the macro.
