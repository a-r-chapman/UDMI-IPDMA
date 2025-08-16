# Create output folder for plots
dir.create("Figures/Eggers", showWarnings = FALSE, recursive = TRUE)

# Define terms
terms <- c("Type 1 MI", "Type 2 MI", "Acute myocardial injury", "Chronic myocardial injury")
times <- c("1y", "5y")
outcome <- "MACE"

# Create all combinations of terms and times
model_keys <- expand.grid(
        term = c("Type 1 MI", "Type 2 MI", "Acute myocardial injury", "Chronic myocardial injury"),
        time = c("1y", "5y"),
        stringsAsFactors = FALSE
) |> 
        dplyr::mutate(model_key = glue("{term} MACE {time}")) |>
        dplyr::pull(model_key)

# Initialize results list
bias_results <- purrr::map_dfr(model_keys, function(key) {
        
        if (!key %in% names(res_list_named)) {
                message(glue("⚠️ Model not found in res_list_named: {key} — skipping."))
                return(NULL)
        }
        
        model <- res_list_named[[key]]
        
        # Egger's regression test
        reg <- tryCatch({
                regtest(model, model = "lm")
        }, error = function(e) NULL)
        
        # Trim-and-fill model
        tf_model <- tryCatch({
                trimfill(model)
        }, error = function(e) NULL)
        
        # Funnel plot path
        safe_name <- gsub(" ", "_", key)
        plot_path <- glue("Figures/Eggers/funnel_{safe_name}.png")
        
        # Save funnel plot
        png(plot_path, width = 800, height = 600)
        tryCatch({
                funnel(tf_model, main = key)
                legend("bottomright", legend = c("Observed", "Filled (imputed)"), col = c("black", "red"), pch = 1)
        }, error = function(e) message(glue("❌ Funnel plot error: {key}")))
        dev.off()
        
        # Return row
        tibble(
                model_name = key,
                egger_intercept = if (!is.null(reg)) reg$int else NA_real_,
                egger_pval = if (!is.null(reg)) reg$pval else NA_real_,
                bias_flag = if (!is.null(reg)) reg$pval < 0.10 else NA,
                k_observed = model$k,
                k_after_trimfill = if (!is.null(tf_model)) tf_model$k else NA_integer_,
                funnel_plot_path = plot_path
        )
})

# View results
print(bias_results)
