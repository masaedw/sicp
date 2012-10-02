#!/usr/bin/env gosh
;; -*- coding: utf-8 mode: scheme -*-

(use srfi-1)
(load "./amb.scm")

(let ((Moor    #;(cons 'Lorna     'Mary))
      (Dowing  (cons 'Melissa   (amb 'Gabrielle 'Lorna 'Rosalind)))
      (Hall    (cons 'Rosalind  (amb 'Gabrielle 'Lorna)))
      (Parker  (cons 'Mary      (amb 'Gabrielle 'Lorna 'Rosalind)))
      (Barncle (cons 'Gabrielle 'Melissa)))
    #;((Moor    (cons (amb 'Lona 'Gabrielle 'Lorna 'Rosalind 'Mary)
                    (amb 'Lona 'Gabrielle 'Lorna 'Rosalind 'Mary)))
     (Dowing  (cons (amb 'Lona 'Gabrielle 'Lorna 'Rosalind 'Mary)
                    (amb 'Lona 'Gabrielle 'Lorna 'Rosalind 'Mary)))
     (Hall    (cons (amb 'Lona 'Gabrielle 'Lorna 'Rosalind 'Mary)
                    (amb 'Lona 'Gabrielle 'Lorna 'Rosalind 'Mary)))
     (Parker  (cons (amb 'Lona 'Gabrielle 'Lorna 'Rosalind 'Mary)
                    (amb 'Lona 'Gabrielle 'Lorna 'Rosalind 'Mary)))
     (Barncle (cons (amb 'Lona 'Gabrielle 'Lorna 'Rosalind 'Mary)
                    (amb 'Lona 'Gabrielle 'Lorna 'Rosalind 'Mary))))
  (define yacht car)
  (define daughter cdr)

  (require (distinct? (map daughter (list Moor Dowing Hall Parker Barncle))))

  (require (eq? (daughter Moor) 'Mary))
  (require (not (eq? (daughter Barncle) 'Gabrielle)))
  (require (not (eq? (daughter Moor) 'Rona)))
  (require (not (eq? (daughter Hall) 'Rosalind)))
  (require (not (eq? (daughter Dowing) 'Melissa)))
  (require (eq? (daughter Barncle) 'Melissa))
  (require (not (eq? (daughter Parker) 'Gabrielle)))
  (let ((GabrielleFather (car (member 'Gabrielle
                                      (list Moor Dowing Hall Parker Barncle)
                                      (lambda (a b) (eq? a (daughter b)))))))
    (require (eq? (yacht GabrielleFather) (daughter Parker))))
  (print `((Moor ,Moor) (Dowing ,Dowing) (Hall ,Hall) (Parker ,Parker) (Barncle ,Barncle)))
  )

(define (main args)
  0)
