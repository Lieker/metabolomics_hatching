correlationlist <- function(x, interest, mode, y = "input/23gt_rootweightsolA.csv", ex = "exudate"){
  xp_design <- read.csv(y, header = TRUE, fileEncoding = "UTF-8-BOM")
  c0 <- as.data.frame(x)
  f <- rownames(x)
  f <- gsub("X","", f)
  xp_design <- xp_design[xp_design$Number %in% f,]
  if(interest == "solA"){
    c0$solA_UHPLC <- xp_design$Concentration.per.g.FW 
    c <- as.data.frame(cor(c0))
    if(ex == "exudate"){
      if(mode == "+"){
        testRes <- read.csv("output/testRes_c_exup_sol.csv", header = TRUE, fileEncoding = "UTF-8-BOM") %>% column_to_rownames("X")
        names(testRes) <- row.names(testRes)
      } else if(mode == "-"){
        testRes <- read.csv("output/testRes_c_exun_sol.csv", header = TRUE, fileEncoding = "UTF-8-BOM") %>% column_to_rownames("X")
        names(testRes) <- row.names(testRes)
      }
    } else if(ex == "extract"){
      if(mode == "+"){
        testRes <- read.csv("output/testRes_c_extp_sol.csv", header = TRUE, fileEncoding = "UTF-8-BOM") %>% column_to_rownames("X")
        names(testRes) <- row.names(testRes)
      } else if(mode == "-"){
        testRes <- read.csv("output/testRes_c_extn_sol.csv", header = TRUE, fileEncoding = "UTF-8-BOM") %>% column_to_rownames("X")
        names(testRes) <- row.names(testRes)
      }
    }  
    c <- c[c$solA_UHPLC > 0.1,]
    c <- c[order(-c$solA_UHPLC),]
    c <- c[,row.names(c)]
    p <- as.data.frame(testRes)
    p <- p[row.names(p) %in% row.names(c),names(p) %in% row.names(c)]
    p <- p[row.names(c), row.names(c)]
    p <- p[, names(p) == "solA_UHPLC"]
    c$pvalue <- p
    c <- c[c$pvalue < 0.05, names(c) %in% c("pvalue", "solA_UHPLC")]
  } else if(interest == "hatching"){
    c0$hatching <- xp_design$Hatching
    c <- as.data.frame(cor(c0))
    if(ex == "exudate"){
      if(mode == "+"){
        testRes <- read.csv("output/testRes_c_exup_hatching.csv", header = TRUE, fileEncoding = "UTF-8-BOM") %>% column_to_rownames("X")
        names(testRes) <- row.names(testRes)
      } else if(mode == "-"){
        testRes <- read.csv("output/testRes_c_exun_hatching.csv", header = TRUE, fileEncoding = "UTF-8-BOM") %>% column_to_rownames("X")
        names(testRes) <- row.names(testRes)
      }
    } else if(ex == "extract"){
      if(mode == "+"){
        testRes <- read.csv("output/testRes_c_extp_hatching.csv", header = TRUE, fileEncoding = "UTF-8-BOM") %>% column_to_rownames("X")
        names(testRes) <- row.names(testRes)
      } else if(mode == "-"){
        testRes <- read.csv("output/testRes_c_extn_hatching.csv", header = TRUE, fileEncoding = "UTF-8-BOM") %>% column_to_rownames("X")
        names(testRes) <- row.names(testRes)
      }
    }  
    c <- c[order(-c$hatching),]
    c <- c[c$hatching > 0.1,]
    c <- c[,row.names(c)]
    p <- as.data.frame(testRes)
    p <- p[row.names(p) %in% row.names(c), names(p) %in% row.names(c)]
    p <- p[row.names(c), row.names(c)]
    p <- p[, names(p) == "hatching"]
    c$pvalue <- p
    c <- c[c$pvalue < 0.05, names(c) %in% c("pvalue", "hatching")]
  }
  
  c <- c %>% rownames_to_column("feature")
  return(c)
}
