=================
 link-header
=================

.. insert-your badges like that:

.. image:: https://travis-ci.org/40ants/cl-link-header.svg?branch=master
    :target: https://travis-ci.org/40ants/cl-link-header

.. Everything starting from this commit will be inserted into the
   index page of the HTML documentation.
.. include-from

This system helps to parse ``Link`` HTTP header from API responses.
Good example of the usage of this header is the documentation of
GitHub's API:

https://developer.github.com/v3/#link-header

Usage
=====

Library provides two ways of parsing the header. Simplest one is
``link-header:parse`` function:

.. code-block:: common-lisp-repl

   CL-USER> (link-header:parse "<https://api.github.com/user/repos?page=3&per_page=100>; rel=\"next\",
     <https://api.github.com/user/repos?page=50&per_page=100>; rel=\"last\"")
   (:LAST "https://api.github.com/user/repos?page=50&per_page=100" :NEXT
    "https://api.github.com/user/repos?page=3&per_page=100")
   CL-USER> (getf * :next)
   "https://api.github.com/user/repos?page=3&per_page=100"

But if you are using great `Dexador`_ library, to make HTTP requests,
then you can use ``link-header:with-links`` macro to process values,
returned by ``dexador:get``.
   
.. code-block:: common-lisp-repl

   CL-USER> (link-header:with-links (response next-link)
              (dex:get "https://api.github.com/repos/sbcl/sbcl/commits")
              (values (length response) next-link))
   92847 (17 bits, #x16AAF)
   "https://api.github.com/repositories/1890957/commits?page=2"

As you can see, ``next-link`` was bound to the next link from the
header. Also, other parameters are supported:

* ``response``;
* ``status-code``;
* ``headers``;
* ``uri``;
* ``xxx-link`` - any parameter with ``-link`` header will be bound to
  corresponding part of the ``Link`` header or ``nil``.

Here is full example:

.. code-block:: common-lisp-repl
                
   CL-USER> (link-header:with-links (response status-code headers uri next-link last-link)
              (dex:get "https://api.github.com/repos/sbcl/sbcl/commits")
              (values (length response)
                      uri
                      status-code
                      headers
                      next-link
                      last-link))
   92847 (17 bits, #x16AAF)
   #<QURI.URI.HTTP:URI-HTTPS https://api.github.com/repos/sbcl/sbcl/commits>
   200 (8 bits, #xC8, #o310, #b11001000)
   #<HASH-TABLE :TEST EQUAL size 24/60 #x30200AD6F3DD>
   "https://api.github.com/repositories/1890957/commits?page=2"
   "https://api.github.com/repositories/1890957/commits?page=452"



   TEST> (format nil "Blah minor: ~a"
                     100500)
   "Blah minor: 100500"

   
.. _Dexador: http://quickdocs.org/dexador/

