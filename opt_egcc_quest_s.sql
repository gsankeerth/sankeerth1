create or replace PACKAGE OPT_EGCC_QUEST
AS
  PROCEDURE CONFIG_ENBD_PRODUCT(P_ORG                    dtype.Name%type,
                                P_LOGO                   dtype.Name%type,
                                P_NAME                   dtype.Name%type,
                                P_CURR                   dtype.Name%type,
                                P_BIN_PREFIX             dtype.Name%type,
                                P_BIN_RNG_MIN            dtype.Name%type,
                                P_BIN_RNG_MAX            dtype.Name%type,
                                P_REPL_CRD_MIN           dtype.Name%type,
                                P_REPL_CRD_MAX           dtype.Name%type,
                                P_EXP_FOR_NEW            dtype.Counter%type,
                                P_EXP_FOR_RENEW          dtype.Counter%type,
                                P_SUPPL_CRD_REQ          dtype.Tag%type,
                                P_MIN_CR_LIM             dtype.Name%type,
                                P_MAX_CR_LIM             dtype.Name%type,
                                P_IS_FLAT_PLAN           dtype.Name%type DEFAULT stnd.No,
                                P_PURGE_MONTH            dtype.Name%type,
                                P_CLOSE_MONTH            dtype.Name%type,
                                P_MMF_INDICATOR          dtype.Name%type,
                                P_VAT_INT_CAL            dtype.Tag%type,
                                P_DLQ_CNCL_PNTS          dtype.Name%type,
                                P_DLQ_EARN_PNTS          dtype.Name%type,
                                P_DLQ_LVL_PNT_REDEEM     dtype.Name%type,
                                P_LTY_PROD_ENRL_STATUS   dtype.Tag%type,
                                P_REF_PROD_BLCKCD        dtype.Name%type,
                                P_INCL_ACTRF             dtype.Tag%type,
                                P_INCL_PROD_LIST_ACTRF   dtype.Name%type,
                                P_CONTR_SUBTYPE_NAME     dtype.Name%type DEFAULT NULL,
								P_PM_CODE                dtype.Name%type DEFAULT NULL,
								P_CHIP_SCHEME_NAME       dtype.Name%type DEFAULT NULL,
                                P_CODE_2                 dtype.Name%type DEFAULT NULL
                                
  );

 PROCEDURE OPT_EGCC_FLEX(P_ORG                   dtype.Name%type,
                         P_PRODUCT_CODE          dtype.Name%type,
                         P_PCT                   dtype.Name%type,
                         P_DEF_PCT               dtype.Name%type
  );

 procedure DUPLICATE_BC_HANDBOOK_PROD(P_ORG dtype.Name%type,
                                      FROM_LOGO dtype.Name%type,
                                      TO_LOGO DTYPE.Name%type
  );

 PROCEDURE CONFIG_INSTANTIATION_PARM(org        dtype.name%type,
                                     logo       dtype.name%type,
                                     parm_name  dtype.name%type,
                                     parm_type  dtype.name%type,
                                     parm_level dtype.name%type default NULL,
                                     parm_val   dtype.name%type,
									 by_event   dtype.name%type,
                                     IS_EDITABLE   dtype.name%type default stnd.Yes,
									 P_custom_rules Dtype.Name%TYPE default NULL,
                                     P_Stepn Dtype.Counter%TYPE default 0
  );

 PROCEDURE CONFIG_PRODUCT_TRANSFER_OPTION(p_org       dtype.name%type,
                                          from_logo   dtype.name%type,
                                          p_forAll    dtype.name%type DEFAULT 'Y',
                                          to_logo     dtype.LongStr%type DEFAULT NULL
  ); 

 PROCEDURE CONFIG_LIABLITY_PRODS(P_ORG    dtype.Name%type,
                                 P_LOGO   dtype.Name%type,
                                 P_CURR   dtype.Name%type,
                                 P_DAC    dtype.Tag%type default 'Y',
                                 P_BT     dtype.Tag%type default 'Y'
  ); 

 procedure copy_tariff_with_data(p_from_domain_path   dtype.Name%type,
                                        p_to_domain_path     dtype.Name%type,
                                        p_with_data          dtype.Tag%type default stnd.No
  );

 procedure copy_product_option(p_org        dtype.Name%type,
                               p_curr       dtype.Name%type,
                               p_ref_logo   dtype.Name%type,
                               p_logo       dtype.Name%type
  );  

 procedure copy_start_events(p_org        dtype.Name%type,
                             p_curr       dtype.Name%type,
                             p_ref_logo   dtype.Name%type,
                             p_logo       dtype.Name%type,
                             p_excl_wshoper_card dtype.Tag%type default stnd.No
  );
function get_contract_sub_type(p_org  dtype.Name%type, 
                               p_contr_subtype_name  dtype.Name%type)
                               return dtype.RecordId %type;

procedure DUPLICATE_PCT(p_org                     dtype.Name%type,
                        p_curr                    dtype.Name%type,
                        p_ref_logo                dtype.Name%type,
                        p_logo                    dtype.Name%type
  );

 procedure copy_product_configuration(p_org                     dtype.Name%type,
                                      p_curr                    dtype.Name%type,
                                      p_ref_logo                dtype.Name%type,
                                      p_logo                    dtype.Name%type,
                                      p_name                    dtype.Name%type,
                                      p_contr_subtype_name      dtype.Name%type,
                                      p_max_cr_lim              dtype.Name%type,
                                      p_min_cr_lim              dtype.Name%type,
                                      p_code2                   dtype.Name%type default null,
                                      p_new_trf_domain_name     dtype.Name%type default null,
                                      p_dup_product_option      dtype.Tag%type default stnd.No,
                                      p_dup_start_event         dtype.Tag%type default stnd.No,
                                      p_dup_pct                 dtype.Tag%type default stnd.No,
                                      p_dup_block_code          dtype.Tag%type default stnd.Yes,
                                      p_dup_tariff              dtype.Tag%type default stnd.Yes,
                                      p_dup_tariff_data         dtype.Tag%type default stnd.Yes,
                                      p_incl_actrf              dtype.Tag%type default stnd.No,
                                      p_incl_prod_list_actrf    dtype.Name%type default NULL,
                                      p_exclude_Eshopper        dtype.Tag%type default stnd.No

  );

end OPT_EGCC_QUEST;
