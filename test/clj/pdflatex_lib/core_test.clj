(ns pdflatex-lib.core-test
  (:require [clojure.test :refer :all]
            [pdflatex-lib.core :refer :all]
            [clojure.string :as cstring]))

(deftest test-read-template
  (testing "Test read template"
    
    (let [template-name nil
          result (read-template
                   template-name)]
      
      (is
        (nil?
          result))
      
     )
    
    (let [template-name ""
          result (read-template
                   template-name)]
      
      (is
        (nil?
          result))
      
     )
    
    (let [template-name "non_existing_template.tex"
          result (read-template
                   template-name)]
      
      (is
        (nil?
          result))
      
     )
    
    (let [template-name "default_template.tex"
          result (read-template
                   template-name)]
      
      (is
        (not
          (nil?
            result)
         )
       )
      
      (is
        (string?
          result)
       )
      
     )
    
   ))

(deftest test-replace-variable
  (testing "Test replace variable"
    
    (let [template nil
          variable-name nil
          replacing-content nil
          result (replace-variable
                   template
                   variable-name
                   replacing-content)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [template-name "default_template.tex"
          template (read-template
                     template-name)
          variable-name nil
          replacing-content nil
          result (replace-variable
                   template
                   variable-name
                   replacing-content)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [template-name "default_template.tex"
          template (read-template
                     template-name)
          variable-name "CHANGE_THIS_VARIABLE"
          replacing-content nil
          result (replace-variable
                   template
                   variable-name
                   replacing-content)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (not
          (cstring/index-of
            result
            variable-name))
       )
      
     )
    
    (let [template-name "default_template.tex"
          template (read-template
                     template-name)
          variable-name "CHANGE_THIS_VARIABLE"
          replacing-content "This text was inserted, instead of variable."
          result (replace-variable
                   template
                   variable-name
                   replacing-content)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (string?
          result)
       )
      
      (is
        (number?
          (cstring/index-of
            result
            "This text was inserted, instead of variable."))
       )
      
     )
    
   ))

(deftest test-generate-temporary-folder-name
  (testing "Test generate temporary folder name"
    
    (let [result (generate-temporary-folder-name)]
      (is
        (not
          (nil?
            result))
       )
     )
    
   ))

(deftest test-execute-pdflatex
  (testing "Test execute pdflatex"
    
    (let [prepared-template nil
          result (execute-pdflatex
                   prepared-template)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [prepared-template 1
          result (execute-pdflatex
                   prepared-template)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [prepared-template ""
          result (execute-pdflatex
                   prepared-template)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [prepared-template "Test non latex content"
          result (execute-pdflatex
                   prepared-template)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [template-name "default_template.tex"
          template (read-template
                     template-name)
          variable-name "CHANGE_THIS_VARIABLE"
          replacing-content "This text was inserted, instead of variable."
          prepared-template (replace-variable
                              template
                              variable-name
                              replacing-content)
          result (execute-pdflatex
                   prepared-template)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (bytes?
          result)
       )
      
     )
    
   ))

(deftest test-escape-special-characters
  (testing "Test escape special characters"
    
    (let [template-to-escape nil
          result (escape-special-characters
                   template-to-escape)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (string?
          result)
       )
      
      (is
        (cstring/blank?
          result)
       )
      
     )
    
    (let [template-to-escape ""
          result (escape-special-characters
                   template-to-escape)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (string?
          result)
       )
      
      (is
        (cstring/blank?
          result)
       )
      
     )
    
    (let [template-to-escape "test"
          result (escape-special-characters
                   template-to-escape)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (string?
          result)
       )
      
      (is
        (= result
           "test")
       )
      
     )
    
    (let [template-to-escape "\\ & % $ # _ { } ~ ^"
          result (escape-special-characters
                   template-to-escape)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (string?
          result)
       )
      
      (is
        (= result
           "\\textbackslash \\& \\% \\$ \\# \\_ \\{ \\} \\textasciitilde \\textasciicircum")
       )
      
     )
    
   ))

(deftest test-generate-latex-table
  (testing "Test generate latex table"
    
    (let [table-data nil
          reports-config nil
          selected-language nil
          result (generate-latex-table
                   table-data
                   reports-config
                   selected-language)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (string?
          result)
       )
      
      (is
        (cstring/blank?
          result)
       )
      
     )
    
    (let [table-data [{:test 1}]
          reports-config {:projection [:test1]
                          :labels {:test2 "label"}}
          selected-language nil
          result (generate-latex-table
                   table-data
                   reports-config
                   selected-language)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (string?
          result)
       )
      
      (is
        (= result
           "\\begin{longtable}{  L{140mm}  }\n\\multicolumn{1}{c}{\\cellcolor{white}\\textcolor{black}{}}\\\\\n\\endhead\n\\\\\n\n\\end{longtable}")
       )
      
     )
    
    (let [table-data [{:test "Table data"}]
          reports-config {:projection [:test]
                          :labels {:test "Header label"}}
          selected-language nil
          result (generate-latex-table
                   table-data
                   reports-config
                   selected-language)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (string?
          result)
       )
      
      (is
        (= result
           "\\begin{longtable}{  L{140mm}  }\n\\multicolumn{1}{c}{\\cellcolor{white}\\textcolor{black}{Header label}}\\\\\n\\endhead\nTable data\\\\\n\n\\end{longtable}")
       )
      
     )
    
    (let [table-data [{:test1 "Table data 1" :test2 "Table data 2"}
                      {:test1 "Table data 3" :test2 "Table data 4"}]
          reports-config {:projection [:test1 :test2]
                          :labels {:test1 "Header label 1" :test2 "Header label 2"}}
          selected-language nil
          result (generate-latex-table
                   table-data
                   reports-config
                   selected-language)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (string?
          result)
       )
      
      (is
        (= result
           "\\begin{longtable}{  L{70mm}  L{70mm}  }\n\\multicolumn{1}{c}{\\cellcolor{white}\\textcolor{black}{Header label 1}} & \\multicolumn{1}{c}{\\cellcolor{white}\\textcolor{black}{Header label 2}}\\\\\n\\endhead\nTable data 1 & Table data 2\\\\\nTable data 3 & Table data 4\\\\\n\n\\end{longtable}")
       )
      
     )
    
   ))

(deftest test-generate-latex-single-entity
  (testing "Test generate latex single entity"
    
    (let [entity-data nil
          reports-config nil
          selected-language nil
          result (generate-latex-single-entity
                   entity-data
                   reports-config
                   selected-language)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (string?
          result)
       )
      
      (is
        (cstring/blank?
          result)
       )
      
     )
    
    (let [entity-data {:test 1}
          reports-config {:projection [:test1]
                          :labels {:test2 "label"}}
          selected-language nil
          result (generate-latex-single-entity
                   entity-data
                   reports-config
                   selected-language)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (string?
          result)
       )
      
      (is
        (= result
           "\\begin{longtable}{ R{75mm} L{75mm} }\n\\cellcolor{white}\\textcolor{black}{} &  \\\\\n\n\\end{longtable}")
       )
      
     )
    
    (let [entity-data {:test 1}
          reports-config {:projection [:test]
                          :labels {:test "label"}}
          selected-language nil
          result (generate-latex-single-entity
                   entity-data
                   reports-config
                   selected-language)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (string?
          result)
       )
      
      (is
        (= result
           "\\begin{longtable}{ R{75mm} L{75mm} }\n\\cellcolor{white}\\textcolor{black}{label} & 1 \\\\\n\n\\end{longtable}")
       )
      
     )
    
    (let [entity-data {:test1 1 :test 2}
          reports-config {:projection [:test1 :test2]
                          :labels {:test1 "label1"
                                   :test2 "label2"}}
          selected-language nil
          result (generate-latex-single-entity
                   entity-data
                   reports-config
                   selected-language)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (string?
          result)
       )
      
      (is
        (= result
           "\\begin{longtable}{ R{75mm} L{75mm} }\n\\cellcolor{white}\\textcolor{black}{label1} & 1 \\\\\n\\cellcolor{white}\\textcolor{black}{label2} &  \\\\\n\n\\end{longtable}")
       )
      
     )
    
   ))

