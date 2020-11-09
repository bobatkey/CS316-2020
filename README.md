# CS316 “Functional Programming”

Welcome to the source code repository for the University of
Strathclyde CS316 “Functional Programming” course.

This is a course designed to teach Haskell to undergraduate
students. The written course materials are available from this
repository. Video lectures and access to the Mattermost forum for this
course are available to Strathclyde students via the course's [MyPlace page](https://classes.myplace.strath.ac.uk/course/view.php?id=15897).

## Getting started

The code in this repository is structured as a
[Stack](https://docs.haskellstack.org/en/stable/README/) project. You
can install Stack by following the instructions on the linked
page. Note that for Windows, the link to the installer appears to be
broken, please try [this link
instead](https://github.com/commercialhaskell/stack/releases/download/v2.3.3/stack-2.3.3-windows-x86_64-installer.exe).

Once you have Slack installed, running the following commands on a
Unix-like system will get the Haskell compiler installed, and the
lecture notes and other programs compiled:

```
$ cd some/nice/directory      # replace 'some/nice/directory' with a real one
$ git clone https://github.com/bobatkey/CS316-2020.git
$ cd CS316-2020
$ stack build
    ... lots of output, might download some things ...
$ stack exec hello-cs316
hello CS316!
$ stack ghci lecture-notes/Week01.hs
    ... will start the interactive Haskell compiler with Week 1's lecture notes loaded ...
```

## Lecture Notes

The lecture notes for this course are intended to accompany the video
lectures, and provide mostly the same information in a searchable and
less linear format that is less bandwidth hungry.

The notes are Haskell files with interleaved code and commentary. You
are encouraged to experiment by loading these files into `ghci` and
editing them. Each section in the files has a few tutorial questions
that we will go through in the tutorial sessions.

- [Week 1](lecture-notes/Week01.hs) : Data and Functions
- [Week 2](lecture-notes/Week02.hs) : Solving Problems by Recursion
- [Week 3](lecture-notes/Week03.hs) : Higher Order Functions
- [Week 4](lecture-notes/Week04.hs) : Patterns of Recursion
- [Week 5](lecture-notes/Week05.hs) : Classes of Types
- [Week 6](lecture-notes/Week06.hs) : Simulating Side Effects
- [Week 7](lecture-notes/Week07.hs) : Monads
- [Week 8](lecture-notes/Week08.hs) : Real Input/Output and Parser Combinators
- [Week 9](lecture-notes/Week09.hs) : TBD
- [Week 10](lecture-notes/Week10.hs) : Infinite Data and Processes

## Coursework Assignments

The coursework assignments will be distributed via this repository
when they are released. Check MyPlace for dates.

- [Exercise 1 : First-order Programming](exercises/Ex1.hs) : Was released Thursday 24th September. The deadline is *Thursday 15th October*. Fill your answers in and submit by uploading the `Ex1.hs` file to MyPlace.
- [Exercise 2 : Higher-order Programming](exercises/Ex2.hs) : Was released Wednesday 14th October. The deadline is *Thursday 29th October*. Fill your answers in and submit by uploading the `Ex2.hs` file to MyPlace.
- [Exercise 3 : Programming Project](exercises/Ex3.hs) : Was released Thursday 5th November. The deadline is *Thursday 3rd December*. Fill your answers in and submit by uploading the files to MyPlace.
