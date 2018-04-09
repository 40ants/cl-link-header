#|
  This file is a part of link-header project.
|#

(defsystem link-header-test
           :author ""
           :license ""
           :class :package-inferred-system
           :pathname "t"
           :depends-on (:link-header
                        "link-header-test/core")
           :description "Test system for link-header"

           :perform (test-op :after (op c)
                             (symbol-call :rove :run c)
                             (clear-system c)))
