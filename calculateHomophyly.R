calculateHomophyly <- function (homophylyEdgeList, anonymous, socioDemographicVariables, nodes) {
  if(anonymous == TRUE) {
    subsetting <- c("id", socioDemographicVariables)
  } else {
    subsetting <- c("Name", socioDemographicVariables)
  }
  
  homophylyLookup <- nodes[,subsetting]
  
  homophylyNetwork <- graph_from_data_frame(homophylyEdgeList,
                                            directed = TRUE,
                                            vertices = homophylyLookup)
  
  assort <- data.frame()
  assort <- setNames(data.frame(matrix(ncol = length(socioDemographicVariables), nrow = 1)), socioDemographicVariables)
  
  if("Gender" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$Gender <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$Gender)), directed = TRUE)
  }
  if("gender" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$gender <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$gender)), directed = TRUE)
  }
  
  if("Tenure" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$Tenure <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$Tenure)), directed = TRUE)
  }
  if("tenure" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$tenure <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$tenure)), directed = TRUE)
  }
  
  if("Location" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$Location <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$Location)), directed = TRUE)
  }
  if("location" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$location <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$location)), directed = TRUE)
  }
  
  if("Wellbeing" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$Wellbeing <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$Wellbeing)), directed = TRUE)
  }
  if("wellbeing" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$wellbeing <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$wellbeing)), directed = TRUE)
  }
  
  if("Ethnicity" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$Ethnicity <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$Ethnicity)), directed = TRUE)
  }
  if("ethnicity" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$ethnicity <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$ethnicity)), directed = TRUE)
  }
  
  if("Age" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$Age <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$Age)), directed = TRUE)
  }
  if("age" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$age <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$age)), directed = TRUE)
  }
  
  if("Education" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$Education <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$Education)), directed = TRUE)
  }
  if("education" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$education <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$education)), directed = TRUE)
  }
  
  if("Role" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$Role <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$Role)), directed = TRUE)
  }
  if("role" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$role <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$role)), directed = TRUE)
  }
  
  if("Network" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$Network <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$Network)), directed = TRUE)
  }
  if("network" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$network <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$network)), directed = TRUE)
  }=
  
  if("Organisation Type" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$'Organisation Type' <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$'Organisation Type')), directed = TRUE)
  }
  if("organisation type" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$'organisation type' <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$'organisation type')), directed = TRUE)
  }
  if("Organisation type" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$'Organisation type' <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$'Organisation type')), directed = TRUE)
  }
  if("organisation Type" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$'organisation Type' <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$'organisation Type')), directed = TRUE)
  }
  
  if("Organisation Size" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$'Organisation Size' <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$'Organisation Size')), directed = TRUE)
  }
  if("organisation size" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$'organisation size' <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$'organisation size')), directed = TRUE)
  }
  if("Organisation size" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$'Organisation size' <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$'Organisation size')), directed = TRUE)
  }
  if("organisation Size" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$'organisation Size' <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$'organisation Size')), directed = TRUE)
  }
  
  if("Group Type" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$'Group Type' <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$'Group Type')), directed = TRUE)
  }
  if("group type" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$'group type' <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$'group type')), directed = TRUE)
  }
  if("Group type" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$'Group type' <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$'Group type')), directed = TRUE)
  }
  if("group Type" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$'group Type' <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$'group Type')), directed = TRUE)
  }
  
  if("Group Size" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$'Group Size' <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$'Group Size')), directed = TRUE)
  }
  if("group size" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$'group size' <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$'group size')), directed = TRUE)
  }
  if("Group size" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$'Group size' <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$'Group size')), directed = TRUE)
  }
  if("group Size" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$'group Size' <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$'group Size')), directed = TRUE)
  }
  
  if("Relational Primacy" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$'Relational Primacy' <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$'Relational Primacy')), directed = TRUE)
  }
  if("relational primacy" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$'relational primacy' <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$'relational primacy')), directed = TRUE)
  }
  if("Relational primacy" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$'Relational primacy' <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$'Relational primacy')), directed = TRUE)
  }
  if("relational Primacy" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$'relational Primacy' <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$'relational Primacy')), directed = TRUE)
  }

  if("Student Teacher Relationship" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$'Student Teacher Relationship' <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$'Student Teacher Relationship')), directed = TRUE)
  }
  if("student teacher relationship" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$'student teacher relationship' <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$'student teacher relationship')), directed = TRUE)
  }
  if("Student teacher relationship" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$'Student teacher relationship' <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$'Student teacher relationship')), directed = TRUE)
  }
  
  if("Line Manager Relationship" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$'Line Manager Relationship' <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$'Line Manager Relationship')), directed = TRUE)
  }
  if("line manager relationship" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$'line manager relationship' <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$'line manager relationship')), directed = TRUE)
  }
  if("Line manager relationship" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$'Line manager relationship' <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$'Line manager relationship')), directed = TRUE)
  }
  
  
  if("School Performance" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$'School Performance' <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$'School Performance')), directed = TRUE)
  }
  if("school performance" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$'school performance' <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$'school performance')), directed = TRUE)
  }
  if("School performance" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$'School performance' <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$'School performance')), directed = TRUE)
  }
  if("school Performance" %in% vertex_attr_names(homophylyNetwork) == TRUE) {
    assort$'school Performance' <- assortativity_nominal(homophylyNetwork, as.numeric(as.factor(V(homophylyNetwork)$'school Performance')), directed = TRUE)
  }

  predictiveChoices <- list()
  nonPredictiveChoices <- list()
  for (column in 1:ncol(assort)) {
    if (assort[column]>0) {
      predictiveChoices[column] <- paste0(names(assort[column]))
      nonPredictiveChoices[column] <- NA
    } else {
      nonPredictiveChoices[column] <- paste0(names(assort[column])) 
      predictiveChoices[column] <- NA
    }
  }
  homophylyOutput <- list(predictiveChoices, nonPredictiveChoices)
}
