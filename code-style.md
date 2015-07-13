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
