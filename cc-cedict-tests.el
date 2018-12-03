(require 'ert)
(require 'cc-cedict)

(ert-deftest cc-cedict ()
  (should (cc-cedict "姊妹")))
