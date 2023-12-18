;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname FinalProject) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; PROBLEM 1:
; 
; Consider a social network similar to Twitter called Chirper. Each user has a name, a note about
; whether or not they are a verified user, and follows some number of people. 
; 
; Design a data definition for Chirper, including a template that is tail recursive and avoids 
; cycles. 
; 
; Then design a function called most-followers which determines which user in a Chirper Network is 
; followed by the most people.


; PROBLEM 2:
; 
; In UBC's version of How to Code, there are often more than 800 students taking 
; the course in any given semester, meaning there are often over 40 Teaching Assistants. 
; 
; Designing a schedule for them by hand is hard work - luckily we've learned enough now to write 
; a program to do it for us! 
; 
; Below are some data definitions for a simplified version of a TA schedule. There are some 
; number of slots that must be filled, each represented by a natural number. Each TA is 
; available for some of these slots, and has a maximum number of shifts they can work. 
; 
; Design a search program that consumes a list of TAs and a list of Slots, and produces one
; valid schedule where each Slot is assigned to a TA, and no TA is working more than their 
; maximum shifts. If no such schedules exist, produce false. 
;
; You should supplement the given check-expects and remember to follow the recipe!
