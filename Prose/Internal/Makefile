
generated =					\
	ALetter.hs				\
	GraphemeBreakTest.hs			\
	Grapheme_Extend.hs			\
	Hebrew_Letter.hs			\
	Katakana.hs				\
	LV.hs					\
	LVT.hs					\
	MidNum.hs				\
	NormalizationTest.hs			\
	Numeric.hs				\
	SpacingMark.hs				\
	WordBreakTest.hs

DERIVE = ./derive

all: $(generated)

clean:
	rm -f $(generated)

.PHONY: clean

ALetter.hs:
	$(DERIVE) set ../../data/WordBreakProperty.txt ALetter

GraphemeBreakTest.hs:
	$(DERIVE) break-test ../../data-test/GraphemeBreakTest.txt GraphemeBreakTest

Grapheme_Extend.hs:
	$(DERIVE) set ../../data/DerivedCoreProperties.txt Grapheme_Extend

Hebrew_Letter.hs:
	$(DERIVE) set ../../data/WordBreakProperty.txt Hebrew_Letter

Katakana.hs:
	$(DERIVE) set ../../data/WordBreakProperty.txt Katakana

LV.hs:
	$(DERIVE) set ../../data/GraphemeBreakProperty.txt LV

LVT.hs:
	$(DERIVE) set ../../data/GraphemeBreakProperty.txt LVT

MidNum.hs:
	$(DERIVE) set ../../data/WordBreakProperty.txt MidNum

NormalizationTest.hs:
	$(DERIVE) norm-test ../../data-test/NormalizationTest.txt NormalizationTest

Numeric.hs:
	$(DERIVE) set ../../data/WordBreakProperty.txt Numeric

SpacingMark.hs:
	$(DERIVE) set ../../data/GraphemeBreakProperty.txt SpacingMark

WordBreakTest.hs:
	$(DERIVE) break-test ../../data-test/WordBreakTest.txt WordBreakTest

