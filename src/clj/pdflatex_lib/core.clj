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

(defn escape-special-characters
  "Escape special characters"
  [template-to-escape]
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
                              ["^" "\\textasciicircum"]]
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
      @escaping-template)
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
                               property-keyword)]
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
            (escape-special-characters
              (str
                property-label))
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
                                            property-value))]
            (when (not= i
                        0)
              (swap!
                table-data-a
                str
                " & "))
            (swap!
              table-data-a
              str
              (escape-special-characters
                (str
                  property-value-formated))
             ))
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
                                          property-value))]
          (swap!
            entity-data-a
            str
            "\\cellcolor{"
            header-background-color
            "}\\textcolor{"
            header-text-color
            "}{"
            (escape-special-characters
              (str
                property-label))
            "}"
            " & "
            (escape-special-characters
              (str
                property-value-formated))
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

