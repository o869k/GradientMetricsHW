run_ordinal_regression_effect <- function (x) {
    model = polr(as.formula(paste('answer~ ', paste0(x,collapse = "+"))), data = dataset, Hess = TRUE)
    ctable <- coef(summary(model)) ## store table
    p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 ## calculate and store p values
    ctable <- cbind(ctable, "p value" = p) ## combined table
    print(ctable)
    plot(Effect(focal.predictors = x,model))
}

generate_optimal_message <- function (x) {
    x = merge(x,cj_prof)
    test_predictions = predict(rf_model,x,"prob")
    cat("Porbability to be Very Likley:")
    cat(max(test_predictions[,4]))
    cat("\nMessage:\n")
    print(cj_prof[which.max(test_predictions[,4]),])
    cat("Porbability to be Somewhat Likley:")
    cat(max(test_predictions[,3]))
    cat("\nMessage:\n")
    print(cj_prof[which.max(test_predictions[,3]),])
}

effect_of_survey_data <- function(x) {
    i = which(colnames(results)==x)
    results_tmp = rbind(data.frame(Term="Very likely",x=results[,i],Prob=results$max_Very_likely,stringsAsFactors = F),
                        data.frame(Term="Somewhat likely",x=results[,i],Prob=results$max_Somewhat_likely,stringsAsFactors = F),
                        data.frame(Term="Very unlikely",x=results[,i],Prob=results$max_Very_unlikely,stringsAsFactors = F),
                        data.frame(Term="Somewhat unlikely",x=results[,i],Prob=results$max_Somewhat_unlikely,stringsAsFactors = F))
    orig_idx = which(colnames(survey_data)==colnames(results)[i])
    lables_levels = gsub("\\s*\\([^\\)]+\\)","",names(attr(survey_data[,orig_idx][[1]], "labels")))
    if (length(lables_levels)>0) {
        if (length(lables_levels)!=length(levels(results_tmp$x))) {
            lables_levels = c("Did Not Answer",lables_levels)
        }
        levels(results_tmp$x) = gsub('\\s|\\/','\n',lables_levels)
    }
    results_tmp$Term = factor(results_tmp$Term,levels=c("Very unlikely","Somewhat unlikely","Somewhat likely","Very likely"),labels=c("Very unlikely","Somewhat unlikely","Somewhat likely","Very likely"))

    if (length(attr(survey_data[,orig_idx][1][[1]][1],"label"))==1) {
        question_title = survey_data[,orig_idx] %>% map_chr(attr_getter("label"))
    } else {
        question_title = colnames(survey_data)[orig_idx]
    }
    question_title = gsub('(?=(?:.{100})+$)', "\n", question_title, perl = TRUE)

    p = results_tmp %>%
        ggplot(aes(x=x,y=Prob, fill=factor(Term))) +
        geom_boxplot() +
        labs(fill = "Term") +
        theme_bw(base_size = 16) +
        ggtitle(question_title)+
        theme(plot.title = element_text(size = 10, face = "bold",hjust = 0.5),
              axis.text=element_text(size=10))+
        scale_x_discrete()+xlab("")

    plot(p)
}
