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
          selected-language nil
          result (escape-special-characters
                   template-to-escape
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
    
    (let [template-to-escape ""
          selected-language ""
          result (escape-special-characters
                   template-to-escape
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
    
    (let [template-to-escape "test"
          selected-language "english"
          result (escape-special-characters
                   template-to-escape
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
           "test")
       )
      
     )
    
    (let [template-to-escape "\\ & % $ # _ { } ~ ^"
          selected-language "english"
          result (escape-special-characters
                   template-to-escape
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

(deftest test-generate-latex-card-table
  (testing "Test generate latex card table"
    
    (let [table-data nil
          reports-config nil
          selected-language nil
          result (generate-latex-card-table
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
                          :labels {:test2 "label"}
                          :card-columns 1}
          selected-language nil
          result (generate-latex-card-table
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
           "\\begin{longtable}{ R{160mm} } \n\\multicolumn{1}{c}{\\cellcolor{lightblue}} \\\\\n\\endhead\n\\begin{tabular}{ p{75mm} p{75mm} } \n  & \\\\\n\\multicolumn{2}{c}{\\cellcolor{lightblue}} \\\\\n\\hline\n\\multicolumn{1}{| L{75mm}}{\\normalsize{}} &\n\\multicolumn{1}{R{75mm} |}{\\normalsize{}} \\\\\n\\hline\n\\end{tabular}\n\\\\\n\n\\end{longtable}")
       )
      
     )
    
    (let [table-data [{:test "Table data"}]
          reports-config {:projection [:test]
                          :labels {:test "Header label"}
                          :card-columns 2}
          selected-language nil
          result (generate-latex-card-table
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
           "\\begin{longtable}{ R{80mm} R{80mm} } \n\\multicolumn{2}{c}{\\cellcolor{lightblue}} \\\\\n\\endhead\n\\begin{tabular}{ p{36mm} p{36mm} } \n  & \\\\\n\\multicolumn{2}{c}{\\cellcolor{lightblue}} \\\\\n\\hline\n\\multicolumn{1}{| L{36mm}}{\\normalsize{Header label}} &\n\\multicolumn{1}{R{36mm} |}{\\normalsize{Table data}} \\\\\n\\hline\n\\end{tabular}\n\n & \n\n\\end{longtable}")
       )
      
     )
    
    (let [table-data [{:test1 "Table data 1" :test2 "Table data 2"}
                      {:test1 "Table data 3" :test2 "Table data 4"}]
          reports-config {:projection [:test1 :test2]
                          :labels {:test1 "Header label 1" :test2 "Header label 2"}
                          :card-columns 2}
          selected-language nil
          result (generate-latex-card-table
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
           "\\begin{longtable}{ R{80mm} R{80mm} } \n\\multicolumn{2}{c}{\\cellcolor{lightblue}} \\\\\n\\endhead\n\\begin{tabular}{ p{36mm} p{36mm} } \n  & \\\\\n\\multicolumn{2}{c}{\\cellcolor{lightblue}} \\\\\n\\hline\n\\multicolumn{1}{| L{36mm}}{\\normalsize{Header label 1}} &\n\\multicolumn{1}{R{36mm} |}{\\normalsize{Table data 1}} \\\\\n\\hline\n\\multicolumn{1}{| L{36mm}}{\\normalsize{Header label 2}} &\n\\multicolumn{1}{R{36mm} |}{\\normalsize{Table data 2}} \\\\\n\\hline\n\\end{tabular}\n\n & \n\\begin{tabular}{ p{36mm} p{36mm} } \n  & \\\\\n\\multicolumn{2}{c}{\\cellcolor{lightblue}} \\\\\n\\hline\n\\multicolumn{1}{| L{36mm}}{\\normalsize{Header label 1}} &\n\\multicolumn{1}{R{36mm} |}{\\normalsize{Table data 3}} \\\\\n\\hline\n\\multicolumn{1}{| L{36mm}}{\\normalsize{Header label 2}} &\n\\multicolumn{1}{R{36mm} |}{\\normalsize{Table data 4}} \\\\\n\\hline\n\\end{tabular}\n\\\\\n\n\\end{longtable}")
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

(deftest test-save-temporary-image-and-remove-temporary-images
  (testing "Test save temporary image and remove temporary images"
    
    (let [single-base64-png nil
          result (save-temporary-image
                   single-base64-png)]
      
      (is
        (nil?
          result)
       )
      
      (let [image-name-vector nil
            remove-output (remove-temporary-images
                            image-name-vector)]
        
        (is
          (nil?
            remove-output)
         )
        
       )
      
     )
    
    (let [single-base64-png "data:image/png;base64,/9j/4AAQSkZJRgABAgAAAQABAAD/2wBDAAgGBgcGBQgHBwcJCQgKDBQNDAsLDBkSEw8UHRofHh0aHBwgJC4nICIsIxwcKDcpLDAxNDQ0Hyc5PTgyPC4zNDL/2wBDAQkJCQwLDBgNDRgyIRwhMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjL/wAARCAAIAAcDASIAAhEBAxEB/8QAHwAAAQUBAQEBAQEAAAAAAAAAAAECAwQFBgcICQoL/8QAtRAAAgEDAwIEAwUFBAQAAAF9AQIDAAQRBRIhMUEGE1FhByJxFDKBkaEII0KxwRVS0fAkM2JyggkKFhcYGRolJicoKSo0NTY3ODk6Q0RFRkdISUpTVFVWV1hZWmNkZWZnaGlqc3R1dnd4eXqDhIWGh4iJipKTlJWWl5iZmqKjpKWmp6ipqrKztLW2t7i5usLDxMXGx8jJytLT1NXW19jZ2uHi4+Tl5ufo6erx8vP09fb3+Pn6/8QAHwEAAwEBAQEBAQEBAQAAAAAAAAECAwQFBgcICQoL/8QAtREAAgECBAQDBAcFBAQAAQJ3AAECAxEEBSExBhJBUQdhcRMiMoEIFEKRobHBCSMzUvAVYnLRChYkNOEl8RcYGRomJygpKjU2Nzg5OkNERUZHSElKU1RVVldYWVpjZGVmZ2hpanN0dXZ3eHl6goOEhYaHiImKkpOUlZaXmJmaoqOkpaanqKmqsrO0tba3uLm6wsPExcbHyMnK0tPU1dbX2Nna4uPk5ebn6Onq8vP09fb3+Pn6/9oADAMBAAIRAxEAPwDj/Dvhv4lQ/FW6gtZP7L8SyxS3tzPOyCJo3OWYhQyupdgMKGAbsNpIKKKAP//Z"
          result (save-temporary-image
                   single-base64-png)]
      
      (is
        (= (cstring/index-of
             result
             "image_")
           0)
       )
      
      (let [image-name-vector [result]
            remove-output (remove-temporary-images
                            image-name-vector)]
        
        (is
          (cstring/blank?
            (:out remove-output))
         )
        
       )
      
     )
    
    
    
   ))

(deftest test-generate-latex-image-and-remove-temporary-images
  (testing "Test generate latex image and remove temporary images"
    
    (let [base64-png nil
          selected-language nil
          [string-result
           images-name-vector] (generate-latex-image
                                 base64-png
                                 selected-language)]
      
      (is
        (cstring/blank?
          string-result)
       )
      
      (is
        (empty?
          images-name-vector)
       )
      
     )
    
    (let [single-base64-png "data:image/png;base64,/9j/4AAQSkZJRgABAgAAAQABAAD/2wBDAAgGBgcGBQgHBwcJCQgKDBQNDAsLDBkSEw8UHRofHh0aHBwgJC4nICIsIxwcKDcpLDAxNDQ0Hyc5PTgyPC4zNDL/2wBDAQkJCQwLDBgNDRgyIRwhMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjL/wAARCAAIAAcDASIAAhEBAxEB/8QAHwAAAQUBAQEBAQEAAAAAAAAAAAECAwQFBgcICQoL/8QAtRAAAgEDAwIEAwUFBAQAAAF9AQIDAAQRBRIhMUEGE1FhByJxFDKBkaEII0KxwRVS0fAkM2JyggkKFhcYGRolJicoKSo0NTY3ODk6Q0RFRkdISUpTVFVWV1hZWmNkZWZnaGlqc3R1dnd4eXqDhIWGh4iJipKTlJWWl5iZmqKjpKWmp6ipqrKztLW2t7i5usLDxMXGx8jJytLT1NXW19jZ2uHi4+Tl5ufo6erx8vP09fb3+Pn6/8QAHwEAAwEBAQEBAQEBAQAAAAAAAAECAwQFBgcICQoL/8QAtREAAgECBAQDBAcFBAQAAQJ3AAECAxEEBSExBhJBUQdhcRMiMoEIFEKRobHBCSMzUvAVYnLRChYkNOEl8RcYGRomJygpKjU2Nzg5OkNERUZHSElKU1RVVldYWVpjZGVmZ2hpanN0dXZ3eHl6goOEhYaHiImKkpOUlZaXmJmaoqOkpaanqKmqsrO0tba3uLm6wsPExcbHyMnK0tPU1dbX2Nna4uPk5ebn6Onq8vP09fb3+Pn6/9oADAMBAAIRAxEAPwDj/Dvhv4lQ/FW6gtZP7L8SyxS3tzPOyCJo3OWYhQyupdgMKGAbsNpIKKKAP//Z"
          base64-png single-base64-png
          selected-language nil
          [string-result
           images-name-vector] (generate-latex-image
                                 base64-png
                                 selected-language)
          compare-result (atom "")]
      
      (doseq [image-name images-name-vector]
        (swap!
          compare-result
          str
          "\n\\includegraphics[width=150mm]{"
          image-name
          "}"))
      
      (is
        (= string-result
           @compare-result)
       )
      
      (is
        (not
          (empty?
            images-name-vector))
       )
      
      (let [image-name-vector images-name-vector
            remove-output (remove-temporary-images
                            image-name-vector)]
        
        (is
          (cstring/blank?
            (:out remove-output))
         )
        
       )
      
     )
    
    (let [single-base64-png "data:image/png;base64,/9j/4AAQSkZJRgABAgAAAQABAAD/2wBDAAgGBgcGBQgHBwcJCQgKDBQNDAsLDBkSEw8UHRofHh0aHBwgJC4nICIsIxwcKDcpLDAxNDQ0Hyc5PTgyPC4zNDL/2wBDAQkJCQwLDBgNDRgyIRwhMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjL/wAARCAAIAAcDASIAAhEBAxEB/8QAHwAAAQUBAQEBAQEAAAAAAAAAAAECAwQFBgcICQoL/8QAtRAAAgEDAwIEAwUFBAQAAAF9AQIDAAQRBRIhMUEGE1FhByJxFDKBkaEII0KxwRVS0fAkM2JyggkKFhcYGRolJicoKSo0NTY3ODk6Q0RFRkdISUpTVFVWV1hZWmNkZWZnaGlqc3R1dnd4eXqDhIWGh4iJipKTlJWWl5iZmqKjpKWmp6ipqrKztLW2t7i5usLDxMXGx8jJytLT1NXW19jZ2uHi4+Tl5ufo6erx8vP09fb3+Pn6/8QAHwEAAwEBAQEBAQEBAQAAAAAAAAECAwQFBgcICQoL/8QAtREAAgECBAQDBAcFBAQAAQJ3AAECAxEEBSExBhJBUQdhcRMiMoEIFEKRobHBCSMzUvAVYnLRChYkNOEl8RcYGRomJygpKjU2Nzg5OkNERUZHSElKU1RVVldYWVpjZGVmZ2hpanN0dXZ3eHl6goOEhYaHiImKkpOUlZaXmJmaoqOkpaanqKmqsrO0tba3uLm6wsPExcbHyMnK0tPU1dbX2Nna4uPk5ebn6Onq8vP09fb3+Pn6/9oADAMBAAIRAxEAPwDj/Dvhv4lQ/FW6gtZP7L8SyxS3tzPOyCJo3OWYhQyupdgMKGAbsNpIKKKAP//Z"
          base64-png [single-base64-png
                      single-base64-png]
          selected-language nil
          [string-result
           images-name-vector] (generate-latex-image
                                 base64-png
                                 selected-language)
          compare-result (atom "")]
      
      (doseq [image-name images-name-vector]
        (swap!
          compare-result
          str
          "\n\\includegraphics[width=150mm]{"
          image-name
          "}"))
      
      (is
        (= string-result
           @compare-result)
       )
      
      (is
        (not
          (empty?
            images-name-vector))
       )
      
      (let [image-name-vector images-name-vector
            remove-output (remove-temporary-images
                            image-name-vector)]
        
        (is
          (cstring/blank?
            (:out remove-output))
         )
        
       )
      
     )
    
   ))

