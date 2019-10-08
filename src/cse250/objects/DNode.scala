/**
 * cse250.objects.DNode.scala
 *
 * Copyright 2019 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * DO NOT MODIFY THIS FILE
 */
package cse250.objects

class DNode[A](var value: A, var prev: DNode[A], var next: DNode[A])
