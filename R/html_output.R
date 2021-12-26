t_html <- list(
  tar_file(s_readme_rmd, "README.Rmd"),
  tar_file(readme_md, {
    siteconf
    sitefiles
    s_readme_html
    s_readme_rmd
    ptrr::render_readme(s_readme_rmd)
  }),
  tar_file(siteconf, "_site.yml"),
  tar_file(sitefiles, "site"),
  # https://github.com/jdblischak/workflowr/issues/238#issuecomment-782024069
  tar_file(s_readme_html, command = {!! tar_knitr_deps_expr("README.Rmd")
    siteconf
    sitefiles
    rmarkdown::render_site(s_readme_rmd)
    "docs/README.html"}),
  tar_file(s_index_html, command = {
    s_readme_html
    s_readme_rmd
    # rmarkdown::render_site(s_index_rmd)
    file.copy("docs/README.html", "docs/index.html", overwrite = TRUE)
    "docs/index.html"}),
  tar_file(s_pokladnachecks_rmd, "s_pokladnachecks.Rmd"),
  tar_file(s_pokladnachecks_html, command = {!! tar_knitr_deps_expr("s_pokladnachecks.Rmd")
    siteconf
    sitefiles
    rmarkdown::render_site(s_pokladnachecks_rmd)
    "docs/s_pokladnachecks.html"}),
  tar_file(s_pokladna_rmd, "s_pokladna.Rmd"),
  tar_file(s_pokladna_html, command = {!! tar_knitr_deps_expr("s_pokladna.Rmd")
    siteconf
    sitefiles
    rmarkdown::render_site(s_pokladna_rmd, output_format = "bookdown::html_document2")
    "docs/s_pokladna.html"}),
  tar_file(s_doc_rmd, "s_doc.Rmd"),
  tar_file(s_doc_html, command = {!! tar_knitr_deps_expr("s_doc.Rmd")
    siteconf
    sitefiles
    rmarkdown::render_site(s_doc_rmd)
    "docs/s_doc.html"}),
  tar_file(s_output_rmd, "s_output.Rmd"),
  tar_file(s_output_html, command = {!! tar_knitr_deps_expr("s_output.Rmd")
    siteconf
    sitefiles
    rmarkdown::render_site(s_output_rmd)
    "docs/s_output.html"}),
  tar_file(s_dev_rmd, "dev.Rmd"),
  tar_file(s_dev_html, command = {!! tar_knitr_deps_expr("dev.Rmd")
    siteconf
    sitefiles
    rmarkdown::render_site(s_dev_rmd)
    "docs/dev.html"}),
  tar_file(s_analyza_rmd, "s_analyza.Rmd"),
  tar_file(s_analyza_docx, command = {!! tar_knitr_deps_expr("s_analyza.Rmd")
    fs::file_move("_site.yml", "__site.yml")
    siteconf
    sitefiles
    rmarkdown::render(s_analyza_rmd)
    fs::file_move("__site.yml", "_site.yml")
    "s_analyza.docx"})
)
