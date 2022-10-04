data("Titanic")

##-- differeecn5t between observed and expected (res) plot above and below line of independence, association plot
strucplot(Titanic)
strucplot(Titanic, core = struc_assoc, shade = T)

vignette("strucplot")

##-- pairs double decker plots
pairs(Titanic, highlighting = 2, diag_panel = pairs_diagonal_mosaic,
             diag_panel_args = list(fill = grey.colors))

##-- differeecn5t between observed and expected (res) plot above and below line of independence, association plot
assoc(Titanic, pop = FALSE)

##-- spec colors in mosaic plot shading 

(ucb <- margin.table(UCBAdmissions, 1:2))
#Gender
#Admit      Male Female
#Admitted 1198    557
#Rejected 1493   1278

(fill_colors <- matrix(c("dark cyan","gray","gray","dark magenta"), ncol = 2))
#[,1]        [,2]
#[1,] "dark cyan" "gray"
#[2,] "gray"      "dark magenta"
 
mosaic(ucb, gp = gpar(fill = fill_colors, col = 0))

##-- alter mosaic shading options
haireye <- margin.table(HairEyeColor, 1:2)
mosaic(haireye, gp = shading_hsv)

mosaic(haireye, gp = shading_hcl)
mosaic(haireye, gp = shading_hcl, gp_args = list(h = c(130, 43), c = 100, l = c(90, 70)))


mosaic(Titanic, subset = Sex == "Female", gp = shading_max)