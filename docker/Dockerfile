FROM rocker/tidyverse@sha256:4bb9be025ad7a13b46a44f6a5bc9c604d2d5814fa37625d6ad36897777fc5280

# My R packages
RUN install2.r --error rstan\
                       brms \
                       lme4 \
                       leaps \
                       caret \
                       glmnet \
                       GGally \
                       here \
                       MuMIn

RUN apt-get update \
	&& apt-get install -y --no-install-recommends \
		vim \
		zsh \
		texlive \
		texlive-base \
		texlive-bibtex-extra \
		texlive-extra-utils \
		texlive-font-utils \
		texlive-fonts-extra \
		texlive-fonts-recommended \
		texlive-full \
		texlive-games \
		texlive-latex-base \
		texlive-latex-extra \
		texlive-latex-recommended \
		texlive-luatex \
		texlive-metapost \
		texlive-music \
		texlive-pictures \
		texlive-pstricks \
		texlive-publishers \
		texlive-science \
		texlive-xetex \
                texlive-xetex \
                texlive-plain-generic \
                texlive-latex-extra \
 		&& apt-get clean
