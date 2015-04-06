# Sleep Raster Plotter

Simple R package for creating raster plots based on temporal data.

# Input File Specifications
see [Input File Documentation](https://github.com/pmanko/raster.plotter/blob/master/doc/INPUT_SPECIFICATIONS.md)

# Install Instructions:

1. Install R (taken from http://blogs.helsinki.fi/bioinformatics-viikki/documentation/getting-started-with-r-programming/installingrlatest/#CentOS

        Installing the latest R on CentOS:

        Add the latest EPEL repository which you can find from here. Don’t forget to add the 64 bit f you are using a 64 bit OS. I have a CentOS release 5.8, 64 bit (Check the Ubuntu installation section of this document if you don’t know your Linux distribution or whether it is 64 or 32 bit ) and I used the following script to add the proper repository:

        $ sudo rpm -Uvh http://www.nic.funet.fi/pub/mirrors/fedora.redhat.com/pub/epel/5/x86_64/epel-release-5-4.noarch.rpm

        Then install R using this script:

        $ sudo yum install R

    Another source: http://datamgmt.com/installing-r-and-rstudio-on-redhat-or-centos-linux/
2. Check to make sure R installed:
     a. open terminal and run ` R --version`
         output should look something like:

            R version 3.0.0 (2013-04-03) -- "Masked Marvel"
            Copyright (C) 2013 The R Foundation for Statistical Computing
            Platform: i686-pc-linux-gnu (32-bit)

            R is free software and comes with ABSOLUTELY NO WARRANTY.
            You are welcome to redistribute it under the terms of the
            GNU General Public License versions 2 or 3.
            For more information about these matters see
            http://www.gnu.org/licenses/.

3. Install required packages:
    a. open terminal and run:
        `R -e 'install.packages(c("RJSONIO", "xts", "ggplot2", "plyr", "scales", "grid"))'`
    b. after above command is completed, run (using the filepath to the file I'm attaching):
        `R -e 'install.packages("<path to folder containing package>/dsm.raster.plot_0.1.tar.gz", repos=NULL)'`

4. Run raster plotter:
    a. open terminal and run:
        `R -e "library(dsm.raster.plot); plot_raster('<path to json input file>/<filename>.json')"`
    b. An image should be created in the path set in the json file.
