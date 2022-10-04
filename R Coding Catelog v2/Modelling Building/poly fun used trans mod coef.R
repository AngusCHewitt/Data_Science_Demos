## using the poly fun to split year into 3 orthogonal cofficients (similar to PCA)
library(gapminder)


lm(lifeExp ~ poly(year, 3), data = gapminder)
