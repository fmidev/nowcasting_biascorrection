FROM centos:8

# No rpm yet for smartmet open 8

RUN echo $'[smartmet-open]\n\
name=FMI SmartMet\n\
baseurl=https://download.fmi.fi/smartmet-open/rhel/8/$basearch/\n\
enabled=1\n\
gpgcheck=0\n\
metadata_expire=5\n\
proxy=http://wwwcache.fmi.fi:8080\n\
\n\
[smartmet-open-noarch]\n\
name=FMI SmartMet\n\
baseurl=https://download.fmi.fi/smartmet-open/rhel/8/noarch/\n\
enabled=0\n\
gpgcheck=0\n\
metadata_expire=5' > /etc/yum.repos.d/smartmet-open.repo

RUN dnf -y install dnf-plugins-core && \
    dnf config-manager --set-enabled PowerTools && \
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
CMD Rscript ./kriging_er_correct.R $INPUT $OUTPUT
