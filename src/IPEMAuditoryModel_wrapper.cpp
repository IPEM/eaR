#include <Rcpp.h>
#include <string.h>

#include <iostream>
#include <fstream>
#include <stdio.h>

extern "C" {
    #include "IPEMAuditoryModel.h"
    #include "wav.h"
}

using namespace Rcpp;

void writeWavFile(int sample_rate,const char* file_name , NumericVector wav){
    //https://faculty.fiu.edu/~wgillam/wavfiles.html
    //https://faculty.fiu.edu/~wgillam/Misc/sinewave.c

    int n = wav.size();

    struct wav_info w;
    w.num_channels=1;
    w.bits_per_sample=16;
    w.sample_rate=sample_rate;
    w.num_samples=n;

    FILE* fp = fopen(file_name,"wb");

    write_wav_hdr(&w,fp);

    //int_fast32_t M = 0x7FFF;

    int_fast32_t sample[1];
    for(int i = 0; i < n; i++){
        sample[0] = wav[i];
        write_wav_sample(&w,fp,sample);
    }
    fclose(fp);
}

NumericVector read_filter_frequencies(){
    //Read filter_frequencies from file
    NumericVector filter_freq_vec;
    std::fstream filter_frequencies_file;
    filter_frequencies_file.open("FilterFrequencies.txt",std::ios::in);  // open a file to perform write operation using file object
    if (filter_frequencies_file.is_open()){   //checking whether the file is open
        std::string line;
        while(getline(filter_frequencies_file, line)){  //read data from file object and put it into string.
            double value = atof(line.c_str());
            filter_freq_vec.push_back(value);
        }
        filter_frequencies_file.close();   //close the file object.
    }
    return filter_freq_vec;
}

NumericMatrix read_auditory_nerve_image(int inNumOfChannels){
    int row_count = 0;
    std::fstream activation_file;
    activation_file.open("AuditoryNerveImage",std::ios::in);  // open a file to perform write operation using file object
    if (activation_file.is_open()){   //checking whether the file is open
        std::string line;
        while(getline(activation_file, line)){  //read data from file object and put it into string.
            row_count++;
        }
    }
    activation_file.close();   //close the file object.

    int col_count=0;
    NumericMatrix activation_mat(row_count, inNumOfChannels);
    activation_file.open("AuditoryNerveImage",std::ios::in);  // open a file to perform write operation using file object
    if (activation_file.is_open()){   //checking whether the file is open
        std::string line;
        row_count = 0;

        while(getline(activation_file, line)){  //read data from file object and put it into string.
            std::istringstream iss(line);
            std::string number;
            col_count = 0;

            while ( getline( iss, number, ' ' ) ) {
                double value = atof(number.c_str());
                activation_mat(row_count,col_count) = value;
                col_count++;
            }
            row_count ++;
        }
    }
    activation_file.close();   //close the file object

    return activation_mat;
}


//' Calculates Auditory Nerve Images
//'
//' @param wav a sampled mono signal
//' @param inNumOfChannels is the number of channels for decimation
//' @param inFirstFreq frequency of first channel (in critical band units)
//' @param inFreqDist the frequency bandwidth of the "auditory filter"
//' @param inSampleFrequency is the sample frequency of the input signal
//' @return Auditory Image + additional files
// [[Rcpp::export]]
List ear_process(NumericVector wav,long inNumOfChannels, double inFirstFreq, double inFreqDist, double inSampleFrequency) {

    long inSoundFileFormat = 0;
    const char* inInputFilePath = ".";
    const char* inInputFileName = "temp_ani_input.wav";
    const char* inOutputFilePath = ".";
    const char* inOutputFileName = "AuditoryNerveImage";


    writeWavFile(inSampleFrequency,inInputFileName,wav);

    //process the wav file

    IPEMAuditoryModel_Setup(inNumOfChannels, inFirstFreq, inFreqDist, inInputFileName, inInputFilePath,
                                   inOutputFileName, inOutputFilePath, inSampleFrequency,
                                   inSoundFileFormat);
    IPEMAuditoryModel_Process();

    NumericVector filter_frequencies = read_filter_frequencies();
    NumericMatrix auditory_nerve_image = read_auditory_nerve_image(inNumOfChannels);

    //Cleanup
    if( remove( "AuditoryNerveImage" ) != 0 ) perror( "Error deleting AuditoryNerveImage" );
    if( remove( "FilterFrequencies.txt" ) != 0 ) perror( "Error deleting FilterFrequencies.txt" );
    if( remove( "decim.dat" ) != 0 ) perror( "Error deleting decim.dat" );
    if( remove( "filters.dat" ) != 0 ) perror( "Error deleting filters.dat" );
    if( remove( "lpf.dat" ) != 0 ) perror( "Error deleting lpf.dat" );
    if( remove( "eef.dat" ) != 0 ) perror( "Error deleting eef.dat" );
    if( remove( "omef.dat" ) != 0 ) perror( "Error deleting omef.dat" );
    if( remove( "outfile.dat" ) != 0 ) perror("Error deleting outfile.dat" );
    if( remove( inInputFileName ) != 0 ) perror("Error deleting wav" );


    return List::create(Named("ANIFilterFreqs") = filter_frequencies ,
                        Named("ANIFreq") = inSampleFrequency/2,
                        Named("AuditoryNerveImage") = auditory_nerve_image);

}

