##############################################################################
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014. However, it has been created with the help of the
# GungHo Consortium, whose members are identified at
# https://puma.nerc.ac.uk/trac/GungHo/wiki
##############################################################################

CMAKE ?= cmake

SPECIAL_FFLAGS := $(filter-out -Werror,$(FFLAGS)) # pFUnit isn't that well put together

PFUNIT_SOURCE = $(abspath ../pfunit)
PFUNIT_BUILD = $(BUILD_DIR)/pfunit

include $(ROOT)/include.mk

COMPILER_NAME = $(shell basename $(FC))
ifeq '$(COMPILER_NAME)' 'ifort'
  PFUNIT_COMPILER_ID = Intel
  ifeq ($(shell test $(IFORT_VERSION) -lt 0130000; echo $$?), 0)
    $(error pFUnit will only compile with ifort v13 or later.)
  else ifeq ($(shell test $(IFORT_VERSION) -lt 0140000; echo $$?), 0)
    export CPPFLAGS += -DINTEL_13
    export FPPFLAGS += -DINTEL_13
  endif
else ifeq '$(COMPILER_NAME)' 'gfortran'
  PFUNIT_COMPILER_ID = GNU
  ifeq ($(shell test $(GFORTRAN_VERSION) -lt 040500), 0)
    $(error pFUnit will only compile with gfortran v4.5 or later.)
  endif
else ifeq '$(COMPILER_NAME)' 'nagfor'
  PFUNIT_COMPILER_ID = NAG
else ifeq '$(COMPILER_NAME)' 'xlf'
  PFUNIT_COMPILER_ID = XL
else
  $(error Unrecognised compiler "$(FC)")
endif

.PHONY: pfunit
pfunit: $(PFUNIT_BUILD)/Makefile
	@echo Building pFUnit
	$(Q)$(MAKE) -C $(PFUNIT_BUILD)
	$(Q)$(MAKE) -C $(PFUNIT_BUILD) tests install

$(PFUNIT_BUILD)/Makefile: | $(PFUNIT_BUILD)
	@echo Configuring pFUnit
	$(Q)cd $(PFUNIT_BUILD); $(CMAKE) -DINSTALL_PATH=$(PFUNIT_INSTALL) \
	                                 $(PFUNIT_SOURCE)

$(PFUNIT_BUILD) $(dir $(DRIVER_OBJ) ):
	@echo Creating $@
	$(Q)mkdir $@

$(PFUNIT_INSTALL)/include/driver.F90: pfunit

$(DRIVER_OBJ): $(PFUNIT_INSTALL)/include/driver.F90 testSuites.inc | $(dir $(DRIVER_OBJ) )
	@echo Compiling $@
	$(Q)$(FC) $(SPECIAL_FFLAGS) -c -I$(PFUNIT_INSTALL)/mod -I. \
	          -DBUILD_ROBUST -o $@ $<

.PHONY: clean
clean:
	-rm -rf $(PFUNIT_BUILD)
	-rm -rf $(PFUNIT_INSTALL)
