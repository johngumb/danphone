#!/usr/bin/env python
##################################################
# Gnuradio Python Flow Graph
# Title: Top Block
# Generated: Sun Dec 28 10:39:13 2014
##################################################

from gnuradio import audio
from gnuradio import blocks
from gnuradio import eng_notation
from gnuradio import filter
from gnuradio import gr
from gnuradio.eng_option import eng_option
from gnuradio.filter import firdes
from grc_gnuradio import blks2 as grc_blks2
from optparse import OptionParser
import fcdproplus

class jag_top_block(gr.top_block):

    def __init__(self):
        gr.top_block.__init__(self)

        ##################################################
        # Variables
        ##################################################
        self.samp_rate = samp_rate = 32000

        ##################################################
        # Blocks
        ##################################################
        self.rational_resampler_xxx_0 = filter.rational_resampler_ccc(
                interpolation=1,
                decimation=6,
                taps=None,
                fractional_bw=None,
        )

        # self.fcdproplus_fcdproplus_0 = fcdproplus.fcdproplus("",1)
        # self.fcdproplus_fcdproplus_0.set_lna(1)
        # self.fcdproplus_fcdproplus_0.set_mixer_gain(1)
        # self.fcdproplus_fcdproplus_0.set_if_gain(20)
        # self.fcdproplus_fcdproplus_0.set_freq_corr(0)
        # self.fcdproplus_fcdproplus_0.set_freq(70050000)
        self.fcdproplus_fcdproplus_0 = fcdproplus.fcdproplus("",1)
        self.fcdproplus_fcdproplus_0.set_lna(0)
        self.fcdproplus_fcdproplus_0.set_mixer_gain(0)
        self.fcdproplus_fcdproplus_0.set_if_gain(47) # new board
#        self.fcdproplus_fcdproplus_0.set_if_gain(38) # old board
        self.fcdproplus_fcdproplus_0.set_freq_corr(0)
        #delta=180
        delta=275
        self.fcdproplus_fcdproplus_0.set_freq(21406000+delta)
#        self.fcdproplus_fcdproplus_0.set_freq(21400000)
#        self.fcdproplus_fcdproplus_0.set_freq(29210000)
          

        self.blocks_complex_to_float_0 = blocks.complex_to_float(1)
        self.audio_sink_0 = audio.sink(samp_rate, "skype", True)

        ##################################################
        # Connections
        ##################################################
        self.connect((self.blocks_complex_to_float_0, 0), (self.audio_sink_0, 0))
        self.connect((self.blocks_complex_to_float_0, 1), (self.audio_sink_0, 1))
        self.connect((self.fcdproplus_fcdproplus_0, 0), (self.rational_resampler_xxx_0, 0))
        self.connect((self.rational_resampler_xxx_0, 0), (self.blocks_complex_to_float_0, 0))





    def get_samp_rate(self):
        return self.samp_rate

    def set_samp_rate(self, samp_rate):
        self.samp_rate = samp_rate

if __name__ == '__main__':
	jag_top_block().run()
