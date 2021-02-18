#' simulator
#'
#' @param numer integer length of distribution
#' @param bins integer size of bins for histogram
#'
#' @return a histigram showing CI at the 95% confidence level
#' @examples
#' simulator()
#' @import infer
#' @importFrom dplyr tibble
#' @importFrom stats cor rbeta
#' @export
customised_model_yardstick <- function (){

    kknn_recipe <-
        recipe(formula = sex ~ ., data = penguin_train) %>%
        step_novel(all_nominal(), -all_outcomes()) %>%
        step_dummy(all_nominal(), -all_outcomes()) %>%
        step_zv(all_predictors()) %>%
        step_normalize(all_predictors(), -all_nominal())


    #added flavour
    c_metrics <- yardstick::metric_set(accuracy,
                                       sens, spec,
                                       roc_auc, mn_log_loss)
    #added flavour
    model_control <- tune::control_grid(save_pred = TRUE)



    kknn_spec <-
        nearest_neighbor(neighbors = tune(), weight_func = tune()) %>%
        set_mode("classification") %>%
        set_engine("kknn")

    knn_grid <- dials::grid_regular(parameters(kknn_spec), levels = 5)


    kknn_workflow <-
        workflow() %>%
        add_recipe(kknn_recipe) %>%
        add_model(kknn_spec)

    set.seed(40477)
    # kknn_tune <-tune_grid(kknn_workflow, resamples = , grid = )
    # Changed flavour

    # Lets create our own tune instead

    knn_tune <- tune_grid(
        kknn_spec,
        kknn_recipe,
        resamples = k_folds,
        control = model_control,
        metrics = c_metrics
    )

    modellist <- list("recipe" = kknn_recipe, "metrics" = c_metrics,"control" = model_control,
                      "spec"=kknn_spec, "grid","workflow" = kknn_workflow, "tune"=knn_tune )

return(modellist)
}


