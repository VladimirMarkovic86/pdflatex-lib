(ns pdflatex-lib.core
  (:require [clojure.string :as cstring]
            [utils-lib.core-clj :as cljutils])
  (:import [java.util Date]
           [java.text SimpleDateFormat]
           [java.io FileInputStream
                    FileOutputStream
                    File]
           [java.util Base64]))

(def reports-templates-path
     (atom "resources/reports/templates/"))

(def reports-generated-path
     (atom "resources/reports/"))

(def base64-decoder
     (Base64/Decoder/getDecoder))

(defn read-template
  "Reads latex template from particular path"
  [template-name]
  (when (and template-name
             (string?
               template-name)
             (not
               (cstring/blank?
                 template-name))
         )
    (try
      (let [template-path (str
                            @reports-templates-path
                            template-name)
            template-is (FileInputStream.
                          (File.
                            template-path))
            available-bytes (.available
                              template-is)
            template-byte-array (byte-array
                                  available-bytes)
            read-is (.read
                      template-is
                      template-byte-array)]
        (.close
          template-is)
        (String.
          template-byte-array
          "UTF-8"))
      (catch Exception e
        (println
          (.getMessage
            e))
       ))
   ))

(defn replace-variable
  "Replaces variable in template with passed replacing content parameter"
  [template
   variable-name
   replacing-content]
  (when (and template
             (string?
               template)
             (not
               (cstring/blank?
                 template))
             variable-name
             (string?
               variable-name)
             (not
               (cstring/blank?
                 variable-name))
         )
    (let [replacing-content (or replacing-content
                                "")
          replaced-variable (cstring/replace-first
                              template
                              variable-name
                              replacing-content)]
      replaced-variable))
 )

(defn generate-temporary-folder-name
  "Generates temporary folder name"
  []
  (let [current-date (Date.)
        sdf (SimpleDateFormat.
              "yyyyMMddHHmmssSSS")]
    (.format
      sdf
      current-date))
 )

(defn execute-pdflatex
  "Executes pdflatex command line program on particular template with all variables replaced with real content"
  [prepared-template]
  (when (and prepared-template
             (string?
               prepared-template)
             (not
               (cstring/blank?
                 prepared-template))
         )
    (let [folder-name (generate-temporary-folder-name)
          new-folder (str
                       @reports-generated-path
                       folder-name)
          void (cljutils/execute-shell-command
                 (str
                   "mkdir "
                   new-folder))
          new-template-insance-path (str
                                      new-folder
                                      "/main_file.tex")
          template-os (FileOutputStream.
                        (File.
                          new-template-insance-path))]
      (.write
        template-os
        (.getBytes
          prepared-template
          "UTF-8"))
      (.flush
        template-os)
      (.close
        template-os)
      (println
        (cljutils/execute-shell-command
          [(str
             "cd "
             new-folder)
           "cp -r ../templates/images ./"
           "pdflatex main_file.tex"
           "pdflatex main_file.tex"]))
      (try
        (let [pdf-file-is (FileInputStream.
                            (File.
                              (str
                                new-folder
                                "/main_file.pdf"))
                           )
              available-bytes (.available
                                pdf-file-is)
              pdf-file-byte-array (byte-array
                                    available-bytes)
              read-is (.read
                        pdf-file-is
                        pdf-file-byte-array)]
          (.close
            pdf-file-is)
          pdf-file-byte-array)
        (catch Exception e
          (println
            (.getMessage
              e))
         )
        (finally
          (cljutils/execute-shell-command
            [(str
               "rm -rf "
               new-folder)])
         ))
     ))
 )

(defn is-cyrillic-fn
  "Searches text for serbian cyrillic letters"
  [text]
  (.find
    (re-matcher
      #"(а|б|в|г|д|ђ|е|ж|з|и|ј|к|л|љ|м|н|њ|о|п|р|с|т|ћ|у|ф|х|с|ч|џ|ш|А|Б|В|Г|Д|Ђ|Е|Ж|З|И|Ј|К|Л|Љ|М|Н|Њ|О|П|Р|С|Т|Ћ|У|Ф|Х|Ц|Ч|Џ|Ш)"
      text))
 )

(defn escape-special-characters
  "Escape special characters"
  [template-to-escape
   selected-language]
  (if (and template-to-escape
           (string?
             template-to-escape)
           (not
             (cstring/blank?
               template-to-escape))
       )
    (let [special-characters [["\\" "\\textbackslash"]
                              ["&" "\\&"]
                              ["%" "\\%"]
                              ["$" "\\$"]
                              ["#" "\\#"]
                              ["_" "\\_"]
                              ["{" "\\{"]
                              ["}" "\\}"]
                              ["~" "\\textasciitilde"]
                              ["^" "\\textasciicircum"]
                              [#"(nolinkurlopenbraces(.+)closedbraces)" "\\\\nolinkurl{$2}"]]
          escaping-template (atom template-to-escape)]
      (doseq [[special-character
               escaped-special-character] special-characters]
        (reset!
          escaping-template
          (cstring/replace
            @escaping-template
            special-character
            escaped-special-character))
       )
      (if (= selected-language
             "serbian")
        (if (is-cyrillic-fn
              @escaping-template)
          @escaping-template
          (str
            "\\secondlanguage{"
            @escaping-template
            "}"))
        (if (is-cyrillic-fn
              @escaping-template)
          (str
            "\\secondlanguage{"
            @escaping-template
            "}")
          @escaping-template))
     )
    ""))

(defn generate-latex-table
  "Generate latex table"
  [table-data
   {projection :projection
    labels :labels
    columns :columns}
   selected-language]
  (if (and table-data
           (vector?
             table-data)
           projection
           (vector?
             projection)
           labels
           (map?
             labels))
    (let [table-data-a (atom "\\begin{longtable}{ ")
          column-width (int
                         (/ 140
                            (count
                              projection))
                        )]
      ;; generate columns and specifying their width
      (dotimes [i (count
                    projection)]
        (let [property-keyword (get
                                 projection
                                 i)
              column-config (get
                              columns
                              property-keyword)
              column-alignment (:column-alignment column-config)
              column-alignment (or column-alignment
                                   "L")
              column-config-width (:width column-config)
              column-config-width (or column-config-width
                                      column-width)]
          (swap!
            table-data-a
            str
            " "
            column-alignment
            "{"
            column-config-width
            "mm} "))
       )
      (swap!
        table-data-a
        str
        " }\n")
      ;; generating table header
      (dotimes [i (count
                    projection)]
        (let [property-keyword (get
                                 projection
                                 i)
              column-config (get
                              columns
                              property-keyword)
              header-background-color (:header-background-color column-config)
              header-background-color (or header-background-color
                                          "white")
              header-text-color (:header-text-color column-config)
              header-text-color (or header-text-color
                                    "black")
              property-label (get
                               labels
                               property-keyword)
              property-label-escaped (escape-special-characters
                                       (str
                                         property-label)
                                       selected-language)]
          (when (not= i
                      0)
            (swap!
              table-data-a
              str
              " & "))
          (swap!
            table-data-a
            str
            "\\multicolumn{1}{c}{\\cellcolor{"
            header-background-color
            "}\\textcolor{"
            header-text-color
            "}{"
            property-label-escaped
            "}}"))
        (when (= (count
                   projection)
                 (inc
                   i))
          (swap!
            table-data-a
            str
            "\\\\\n"))
       )
      (swap!
        table-data-a
        str
        "\\endhead\n")
      ;; generating table data
      (doseq [table-row table-data]
        (dotimes [i (count
                      projection)]
          (let [property-keyword (get
                                   projection
                                   i)
                column-config (get
                                columns
                                property-keyword)
                data-format-fn (:data-format-fn column-config)
                column-alignment (:column-alignment column-config)
                column-alignment (or column-alignment
                                     "L")
                property-value (get
                                 table-row
                                 property-keyword)
                property-value-formated (if (and data-format-fn
                                                 (fn?
                                                   data-format-fn))
                                          (data-format-fn
                                            property-value
                                            selected-language)
                                          (str
                                            property-value))
                property-value-escaped (escape-special-characters
                                         (str
                                           property-value-formated)
                                         selected-language)]
            (when (not= i
                        0)
              (swap!
                table-data-a
                str
                " & "))
            (swap!
              table-data-a
              str
              property-value-escaped))
          (when (= (count
                     projection)
                   (inc
                     i))
            (swap!
              table-data-a
              str
              "\\\\\n"))
         ))
      (swap!
        table-data-a
        str
        "\n"
        "\\end{longtable}")
      @table-data-a)
    ""))

(defn generate-latex-card-table
  "Generates latex card table"
  [table-data
   {projection :projection
    labels :labels
    columns :columns
    card-columns :card-columns}
   selected-language]
  (if (and table-data
           (vector?
             table-data)
           projection
           (vector?
             projection)
           labels
           (map?
             labels)
           card-columns
           (number?
             card-columns))
    (let [card-data-a (atom "\\begin{longtable}{ ")
          itr (atom 1)]
      (case card-columns
        1 (swap!
            card-data-a
            str
            "R{160mm} } \n")
        2 (swap!
            card-data-a
            str
            "R{80mm} R{80mm} } \n")
        3 (swap!
            card-data-a
            str
            "R{53mm} R{53mm} R{53mm} } \n")
        4 (swap!
            card-data-a
            str
            "R{40mm} R{40mm} R{40mm} R{40mm} } \n")
        5 (swap!
            card-data-a
            str
            "R{32mm} R{32mm} R{32mm} R{32mm} R{32mm} } \n")
        (swap!
          card-data-a
          str
          "R{160mm} } \n"))
      (swap!
        card-data-a
        str
        "\\multicolumn{"
        card-columns
        "}{c}{\\cellcolor{lightblue}} \\\\\n"
        "\\endhead\n")
      (doseq [card-data table-data]
        (let [card-column-width (case card-columns
                                  1 "75"
                                  2 "36"
                                  3 "22"
                                  4 "16"
                                  5 "12"
                                  "75")]
          (swap!
            card-data-a
            str
            "\\begin{tabular}{ p{"
            card-column-width
            "mm} p{"
            card-column-width
            "mm} } \n"
            "  & \\\\\n"
            "\\multicolumn{2}{c}{\\cellcolor{lightblue}} \\\\\n"
            "\\hline\n")
          (dotimes [i (count
                        projection)]
            (let [property-keyword (get
                                     projection
                                     i)
                  column-config (get
                                  columns
                                  property-keyword)
                  data-format-fn (:data-format-fn column-config)
                  property-label (get
                                   labels
                                   property-keyword)
                  property-label-escaped (escape-special-characters
                                           (str
                                             property-label)
                                           selected-language)
                  property-value (get
                                   card-data
                                   property-keyword)
                  property-value-formated (if (and data-format-fn
                                                   (fn?
                                                     data-format-fn))
                                            (data-format-fn
                                              property-value
                                              selected-language)
                                            (str
                                              property-value))
                  property-value-escaped (escape-special-characters
                                           (str
                                             property-value-formated)
                                           selected-language)
                  font-size (case card-columns
                              1 "normalsize"
                              2 "normalsize"
                              3 "normalsize"
                              4 "small"
                              5 "footnotesize"
                              "normalsize")]
              (swap!
                card-data-a
                str
                "\\multicolumn{1}{| L{"
                card-column-width
                "mm}}{\\"
                font-size
                "{"
                property-label-escaped
                "}} &\n"
                "\\multicolumn{1}{R{"
                card-column-width
                "mm} |}{\\"
                font-size
                "{"
                property-value-escaped
                "}} \\\\\n"
                "\\hline\n"))
           ))
        (swap!
          card-data-a
          str
          "\\end{tabular}\n"
          (if (= (mod @itr
                      card-columns)
                 0)
            "\\\\\n"
            "\n & \n"))
        (swap!
          itr
          inc))
      (swap!
        card-data-a
        str
        "\n"
        "\\end{longtable}")
      @card-data-a)
    ""))

(defn generate-latex-single-entity
  "Generates latex table with data about single entity instance"
  [entity-data
   {projection :projection
    labels :labels
    columns :columns}
   selected-language]
  (if (and entity-data
           (map?
             entity-data)
           projection
           (vector?
             projection)
           labels
           (map?
             labels))
    (let [entity-data-a (atom "\\begin{longtable}{ R{75mm} L{75mm} }\n")
          column-width (int
                         (/ 140
                            (count
                              projection))
                        )]
      (doseq [property-keyword projection]
        (let [column-config (get
                              columns
                              property-keyword)
              header-background-color (:header-background-color column-config)
              header-background-color (or header-background-color
                                          "white")
              header-text-color (:header-text-color column-config)
              header-text-color (or header-text-color
                                    "black")
              data-format-fn (:data-format-fn column-config)
              property-label (get
                               labels
                               property-keyword)
              property-label-escaped (escape-special-characters
                                       (str
                                         property-label)
                                       selected-language)
              property-value (get
                               entity-data
                               property-keyword)
              property-value-formated (if (and data-format-fn
                                               (fn?
                                                 data-format-fn))
                                        (data-format-fn
                                          property-value
                                          selected-language)
                                        (str
                                          property-value))
              property-value-escaped (escape-special-characters
                                       (str
                                         property-value-formated)
                                       selected-language)]
          (swap!
            entity-data-a
            str
            "\\cellcolor{"
            header-background-color
            "}\\textcolor{"
            header-text-color
            "}{"
            property-label-escaped
            "}"
            " & "
            property-value-escaped
            " \\\\\n"))
       )
      (swap!
        entity-data-a
        str
        "\n"
        "\\end{longtable}")
      @entity-data-a)
    ""))

(defn save-temporary-image
  "Saves temporary image before it uses it in pdflatex template
   afterwards should be deleted in execute-pdflatex function"
  [single-base64-png]
  (when (and single-base64-png
             (string?
               single-base64-png))
    (let [random-number (long
                          (* (Math/random)
                             100000000))
          temporary-image-name (str
                                 "image_"
                                 random-number)
          image-base64 (cstring/replace
                         single-base64-png
                         "data:image/png;base64,"
                         "")
          image-byte-array (.decode
                             base64-decoder
                             image-base64)
          new-image-insance-path (str
                                   @reports-generated-path
                                   "templates/images/"
                                   temporary-image-name
                                   ".png")
          image-os (FileOutputStream.
                     (File.
                       new-image-insance-path))]
      (.write
        image-os
        image-byte-array)
      (.flush
        image-os)
      (.close
        image-os)
      temporary-image-name))
 )

(defn generate-latex-image
  "Generates latex pdf file with image"
  [base64-png
   selected-language]
  (let [string-result (atom "")
        images-name-vector (atom [])]
    (when (and base64-png
               (vector?
                 base64-png))
      (doseq [single-base64-png base64-png]
        (let [temporary-image-name (save-temporary-image
                                     single-base64-png)]
          (swap!
            string-result
            str
            "\n\\includegraphics[width=150mm]{"
            temporary-image-name
            "}")
          (swap!
            images-name-vector
            conj
            temporary-image-name))
       ))
    (when (and base64-png
               (string?
                 base64-png))
      (let [temporary-image-name (save-temporary-image
                                   base64-png)]
        (swap!
          string-result
          str
          "\n\\includegraphics[width=150mm]{"
          temporary-image-name
          "}")
        (swap!
          images-name-vector
          conj
          temporary-image-name))
     )
    [@string-result
     @images-name-vector]))

(defn remove-temporary-images
  "Removes temporary images"
  [image-name-vector]
  (when (and image-name-vector
             (vector?
               image-name-vector))
    (let [remove-output-a (atom "")]
      (doseq [image-name image-name-vector]
        (let [tmp-image-insance-path (str
                                       @reports-generated-path
                                       "templates/images/"
                                       image-name
                                       ".png")
              remove-output (cljutils/execute-shell-command
                              (str
                                "rm -r "
                                tmp-image-insance-path))]
          (println
            remove-output)
          (swap!
            remove-output-a
            str
            "\n"
            remove-output))
       )
      @remove-output-a))
 )

