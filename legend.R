# Set up your colors and layer names
legend_items <- data.frame(
  label = c("Undisturbed areas\u00B9", 
            "Disturbed areas\u00B2", 
            "Areal disturbances", 
            "Linear disturbances", 
            "Other areal disturbances",
            "Other linear disturbances",
            "Fires (Lightning)", 
            "Fires (Human)", 
            "Fires (Unknown)",
            "Quartz Claims", 
            "Placer Claims", 
            "Mining Claims",
            "Protected areas", 
            "Intact FL 2000", 
            "Intact FL 2020"
  ),
  color = c("#336633", 
            "black", 
            "#660000", 
            "#CC3333", 
            "#FF9966",
            "#FF6600",
            "#996633", 
            "#663300", 
            "pink",
            "#CCCCCC",
            "#666666", 
            "#666666",
            "#699999",
            "#3366FF", 
            "#000066"
  ),
  type = c("polygon", 
           "polygon", 
           "polygon", 
           "line", 
           "polygon", 
           "line",
           "polygon", 
           "polygon", 
           "polygon", 
           "polygon", 
           "polygon", 
           "polygon", 
           "polygon", 
           "polygon", 
           "polygon"
  ),
  stringsAsFactors = FALSE
)

# Save to PNG
png("www/legend.png", width = 600, height = 800, res = 120)

# Setup plot
par(mar = c(1, 1, 1, 1), oma = c(0, 0, 0, 0))
plot(NULL, xlim = c(0, 1), ylim = c(0, nrow(legend_items)), xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")

# Add legend items
for (i in seq_len(nrow(legend_items))) {
  #browser()
  y_pos <- (nrow(legend_items) - i )*0.8
  if (legend_items$type[i] == "polygon") {
    rect(0.05, y_pos - 0.3, 0.15, y_pos + 0.3, col = legend_items$color[i], border = NA)
  } else if (legend_items$type[i] == "line") {
    lines(c(0.05, 0.15), c(y_pos, y_pos), col = legend_items$color[i], lwd = 3)
  }
  text(0.3, y_pos, legend_items$label[i], pos = 4, cex = 0.9)
}


# Save the file
dev.off()


