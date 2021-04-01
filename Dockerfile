FROM centos:8

RUN rpm -ivh https://download.fmi.fi/smartmet-open/rhel/8/x86_64/smartmet-open-release-21.3.26-2.el8.fmi.noarch.rpm

RUN dnf -y install dnf-plugins-core && \
    dnf config-manager --set-enabled powertools && \
    dnf -y install epel-release && \
    dnf config-manager --setopt="epel.exclude=eccodes*" --save

RUN dnf -y install R-core \
                   R-devel \
                   gcc-c++ \
                   zlib-devel \
                   libssh2-devel \
                   openssl-devel \
                   libcurl-devel \
                   libxml2-devel \
                   proj-devel \
                   eccodes eccodes-devel \
                   geos-devel \
                   gdal-devel \
    && dnf clean all && rm -rf /var/cache/yum

ENV LC_ALL=C
RUN R -e 'install.packages(c("devtools","sp","rgeos","rgdal","raster","aws.s3"),repos="http://ftp.eenet.ee/pub/cran/",build_vignettes=F)'
RUN R -e 'devtools::install_github(c("mjlaine/fastgrid","harphub/Rgrib2"),build_vignettes=F)'

RUN mkdir /nowcasting_biascorrection
ADD . /nowcasting_biascorrection

WORKDIR /nowcasting_biascorrection

RUN chmod g+rw /nowcasting_biascorrection # allow writes in strict environments

CMD Rscript ./kriging_er_correct.R $INPUT $OUTPUT
