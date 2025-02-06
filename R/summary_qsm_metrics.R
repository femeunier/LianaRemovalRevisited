summary_qsm_metrics <- function (QSMs_path, version = "2.4.1", multiple = FALSE, sbr_normalisation = "treeheight",
          sbl_normalisation = "treeheight", sbd_normalisation = "no",
          cylindercutoff = 0, PCs_path = NA, extension = ".txt", buttress = FALSE,
          thresholdR2 = 0.001, slice_thickness = 0.06, thresholdbuttress = 0.001,
          maxbuttressheight = 7, dtm = NA, r = 5, OUT_path = FALSE)
{
  filenames <- list.files(QSMs_path, pattern = "*.mat", full.names = FALSE)
  unique_tree_ids <- c()
  tree_ids <- c()
  for (i in 1:length(filenames)) {
    id <- strsplit(filenames[i], "_qsm")[[1]][1]
    if (!(id %in% tree_ids)) {
      unique_tree_ids <- append(unique_tree_ids, id)
    }
    tree_ids <- append(tree_ids, id)
  }
  df <- results <- data.frame(X_position = double(), Y_position = double(),
                              dbh_m = double(), tree_height_m = double(), tree_vol_L = double(),
                              trunk_vol_L = double(), branch_len = double(), trunk_h = double(),
                              sba_degrees = double(), sbcs = double(), sbr = double(),
                              sbl = double(), sbd = double(), dhr = double(), `dvr_m-2` = double(),
                              vb55 = double(), `clvr_m-2` = double(), sr = double(),
                              bar = double(), rvr = double(), csh = double(), ch = double(),
                              ce = double(), cdhr = double(), dmr = double(),
                              branch_area = double(),

                              crown_vol = double(), crown_area_conv = double(),
                              crown_length  = double(),crown_area_alpha = double(),
                              Nbranches = double(),
                              crown_base_height  = double(),

                              branch_number_order = double(),
                              branch_length_order = double(),
                              branch_volume_order = double(),
                              branch_area_order = double(),
                              ratio_area_volume_order = double(),

                              branch_number_order1 = double(),
                              branch_length_order1 = double(),
                              branch_volume_order1 = double(),
                              branch_area_order1 = double(),

                              branch_number_order2 = double(),
                              branch_length_order2 = double(),
                              branch_volume_order2 = double(),
                              branch_area_order2 = double()

                              )
  summary <- summary_means <- summary_sds <- cbind(tree_id = character(),
                                                   results)
  for (i in 1:length(unique_tree_ids)) {
    print(paste("processing ", unique_tree_ids[i],"-",round(100*i/length(unique_tree_ids),digits = 1),"%"))
    qsms <- filenames[tree_ids == unique_tree_ids[i]]
    if (!is.na(PCs_path)) {
      pc <- read_tree_pc(paste(PCs_path, unique_tree_ids[i],
                               "_pc", extension, sep = ""))
    }
    else {
      pc <- NA
    }
    trees <- df
    id <- unique_tree_ids[i]
    if (multiple) {
      all_qsms <- LianaRemovalRevisited::read_tree_qsm(paste(QSMs_path, qsms[1],
                                      sep = ""), version)
      qsms <- all_qsms
    }
    for (j in 1:length(qsms)) {
      print(paste("processing ", unique_tree_ids[i], as.character(j)))
      if (multiple) {
        qsm <- qsms[[j]]
      }
      else {
        qsm <- LianaRemovalRevisited::read_tree_qsm(paste(QSMs_path, qsms[j],
                                                          sep = ""), version)
      }
      position <- tree_position_qsm(qsm$cylinder)
      X_position <- position[1]
      Y_position <- position[2]
      dbh <- dbh(qsm$treedata, pc, buttress, thresholdR2,
                 slice_thickness, thresholdbuttress, maxbuttressheight,
                 dtm = dtm, r = r)
      tree_height <- tree_height(qsm$treedata, pc, dtm = dtm,
                                 r = r)
      tree_vol <- tree_volume_qsm(qsm$treedata, qsm$cylinder,
                                  cylindercutoff)
      trunk_vol <- trunk_volume_qsm(qsm$treedata)

      pos <- (qsm$cylinder$BranchOrder > 0 &
                qsm$cylinder$BranchOrder < 3)

      branch_number_order <- length(unique(qsm$cylinder$branch[pos]))
      branch_length_order <- sum(qsm$cylinder$length[pos])
      branch_volume_order <- sum(qsm$cylinder$length[pos]*pi*
                                 qsm$cylinder$radius[pos]**2)
      branch_area_order <- sum(qsm$cylinder$length[pos]*2*pi*
                                   qsm$cylinder$radius[pos])


      pos1 <- (qsm$cylinder$BranchOrder == 1)

      branch_number_order1 <- length(unique(qsm$cylinder$branch[pos1]))
      branch_length_order1 <- sum(qsm$cylinder$length[pos1])
      branch_volume_order1 <- sum(qsm$cylinder$length[pos1]*pi*
                                   qsm$cylinder$radius[pos1]**2)
      branch_area_order1 <- sum(qsm$cylinder$length[pos1]*2*pi*
                                 qsm$cylinder$radius[pos1])

      pos2 <- (qsm$cylinder$BranchOrder == 2)

      branch_number_order2 <- length(unique(qsm$cylinder$branch[pos2]))
      branch_length_order2 <- sum(qsm$cylinder$length[pos2])
      branch_volume_order2 <- sum(qsm$cylinder$length[pos2]*pi*
                                    qsm$cylinder$radius[pos2]**2)
      branch_area_order2 <- sum(qsm$cylinder$length[pos2]*2*pi*
                                  qsm$cylinder$radius[pos2])


      ratio_area_volume_order <- data.frame(branch = qsm$cylinder$branch[pos],
                                            l = qsm$cylinder$length[pos],
                                            r = qsm$cylinder$radius[pos]) %>%
        group_by(branch) %>%
        summarise(ratio = sum(2*pi*r*l)/sum(pi*(r**2)*l),
                  .groups = "keep") %>%
        ungroup() %>% pull(ratio) %>% mean()

      cvol <- qsm$treedata$CrownVolumeConv[1]
      clen <- qsm$treedata$CrownLength[1]
      number_bran <- qsm$treedata$NumberBranches[1]
      cbh <- qsm$treedata$CrownBaseHeight[1]
      ca.alpha <- qsm$treedata$CrownAreaAlpha[1]
      ca.conv <- qsm$treedata$CrownAreaConv[1]
      bra <- qsm$treedata$BranchArea[1]

      branch_len <- total_branch_length_qsm(qsm$treedata)
      trunk_height <- trunk_height_qsm(qsm$treedata)
      sba <- stem_branch_angle_qsm(qsm$branch)
      sbcs <- stem_branch_cluster_size_qsm(qsm$cylinder)
      sbr <- stem_branch_radius_qsm(qsm$cylinder, qsm$treedata,
                                    sbr_normalisation, pc, dtm = dtm, r = r)
      sbl <- stem_branch_length_qsm(qsm$branch, qsm$treedata,
                                    sbl_normalisation, pc, buttress, thresholdR2,
                                    slice_thickness, thresholdbuttress, maxbuttressheight,
                                    dtm, r)
      sbd <- stem_branch_distance_qsm(qsm$cylinder, qsm$treedata,
                                      sbd_normalisation, pc, buttress, thresholdR2,
                                      slice_thickness, thresholdbuttress, maxbuttressheight,
                                      dtm, r)
      dhr <- dbh_height_ratio_qsm(qsm$treedata, pc, buttress,
                                  thresholdR2, slice_thickness, thresholdbuttress,
                                  maxbuttressheight, dtm, r)
      dvr <- dbh_volume_ratio_qsm(qsm$treedata, pc, buttress,
                                  thresholdR2, slice_thickness, thresholdbuttress,
                                  maxbuttressheight, dtm, r)
      vb55 <- volume_below_55_qsm(qsm$cylinder, qsm$treedata)
      clvr <- cylinder_length_volume_ratio_qsm(qsm$treedata)
      sr <- shedding_ratio_qsm(qsm$branch, qsm$cylinder,
                               qsm$treedata)
      bar <- branch_angle_ratio_qsm(qsm$branch)
      rvr <- relative_volume_ratio_qsm(qsm$cylinder, qsm$treedata)
      csh <- crown_start_height_qsm(qsm$treedata, qsm$cylinder,
                                    pc, dtm, r)
      ch <- crown_height_qsm(qsm$treedata, qsm$cylinder,
                             pc, dtm, r)
      ce <- crown_evenness_qsm(qsm$cylinder)
      cdhr <- crown_diameterheight_ratio_qsm(qsm$treedata,
                                             qsm$cylinder, pc, dtm, r)
      dmr <- dbh_minradius_ratio_qsm(qsm$treedata, qsm$cylinder,
                                     pc, buttress, thresholdR2, slice_thickness,
                                     thresholdbuttress, maxbuttressheight, dtm, r)
      tree <- data.frame(X_position = X_position, Y_position = Y_position,
                         dbh_m = dbh, tree_height_m = tree_height, tree_vol_L = tree_vol,
                         trunk_vol_L = trunk_vol, branch_len = branch_len,
                         trunk_h = trunk_height, sba_degrees = sba, sbcs = sbcs,
                         sbr = sbr, sbl = sbl, sbd = sbd, dhr = dhr,
                         `dvr_m-2` = dvr, vb55 = vb55, `clvr_m-2` = clvr,
                         sr = sr, bar = bar, rvr = rvr, csh = csh, ch = ch,
                         ce = ce, cdhr = cdhr, dmr = dmr,
                         crown_vol = cvol,
                         crown_area_alpha = ca.alpha,
                         crown_area_conv = ca.conv,
                         crown_length = clen,branch_area = bra,
                         Nbranches = number_bran,
                         crown_base_height = cbh,
                         branch_number_order,
                         branch_length_order,
                         branch_volume_order,
                         branch_area_order,
                         ratio_area_volume_order,

                         branch_number_order1,
                         branch_length_order1,
                         branch_volume_order1,
                         branch_area_order1,

                         branch_number_order2,
                         branch_length_order2,
                         branch_volume_order2,
                         branch_area_order2

                         )
      trees <- rbind(trees, tree)
    }
    results <- cbind(tree_id = id, trees)
    summary <- rbind(summary, results)
    if (length(qsms) > 1) {
      m <- as.data.frame.list(colMeans(trees))
      s <- as.data.frame.list(sapply(trees, stats::sd,
                                     na.rm = TRUE))
      results_means <- cbind(tree_id = id, m)
      results_sds <- cbind(tree_id = id, s)
      summary_means <- rbind(summary_means, results_means)
      summary_sds <- rbind(summary_sds, results_sds)
      summaries <- list(summary = summary, means = summary_means,
                        sds = summary_sds)
    }
    else {
      summaries <- summary
    }
    if (is.character(OUT_path)) {
      file <- paste(OUT_path, ".csv", sep = "")
      utils::write.csv(summary, file, row.names = FALSE)
      if (length(qsms) > 1) {
        file_means <- paste(OUT_path, "_means.csv",
                            sep = "")
        file_sds <- paste(OUT_path, "_sds.csv", sep = "")
        utils::write.csv(summary_means, file_means,
                         row.names = FALSE)
        utils::write.csv(summary_sds, file_sds, row.names = FALSE)
      }
    }
  }
  return(summaries)
}
