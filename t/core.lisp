(defpackage #:link-header-test/core
  (:use #:cl
        #:link-header/core
        #:rove
        #:hamcrest/rove))
(in-package link-header-test/core)


(deftest test-some-staff
    (testing "Replace this test with real staff."
      (assert-that (foo 1 2)
                   (contains 1 2))))
