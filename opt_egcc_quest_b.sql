create or replace PACKAGE BODY OPT_EGCC_QUEST
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
                P_CODE_2       dtype.Name%type DEFAULT NULL
  )
IS
V_DOMAIN_PATH dtype.name % type;
ProdRec       appl_product%rowtype;
v_fi_id       dtype. Recordid%type;
ContrSubtype  contr_subtype%rowtype;
ServPack      serv_pack%rowtype;
counter      dtype. Name%type;
DomainID      dtype. RecordID       %Type;
RetCode       dtype. Counter          %Type;
RetMsg        dtype. ErrorMessage %Type;
V_CS_VALUE_CODE  dtype. Name%type;
V_SY_HANDBOOK SY_HANDBOOK%ROWTYPE;
V_code DTYPE.NAME%TYPE;
V_CONTR_TYPE_CODE dtype.Name%type := CASE WHEN SUBSTR(P_BIN_PREFIX,1,1) = 4 THEN 'vc' ELSE 'ec' END;
V_CCAT dtype.Tag%type := 'P';
V_SERVICE_CODE dtype.Name%type := '201';
V_CONTR_SUBTYPE_NAME dtype.Name%type := CASE WHEN P_CONTR_SUBTYPE_NAME IS NULL THEN
                                          P_NAME
										ELSE
                                          P_CONTR_SUBTYPE_NAME
                                        END;										  
V_PM_CODE dtype.Name%type := CASE WHEN P_PM_CODE IS NULL THEN
                                  CASE WHEN SUBSTR(P_BIN_PREFIX,1,1) = 4 THEN
                                    P_ORG || '_' || replace(UPPER(P_NAME),' ','_') || '_' || P_LOGO
                                  ELSE
                                    P_ORG || '_' || 'MC' ||'_' || replace(UPPER(P_NAME),' ','_') || '_' || P_LOGO
                                  END
							 ELSE 
							   UPPER(P_PM_CODE)
							 END;  

V_NUMERATION_TYPE dtype.Tag%type := 'C';
V_PLASTIC_CODE dtype.Name%type := CASE WHEN SUBSTR(P_BIN_PREFIX,1,1) = 4 THEN
                                    'V' || P_ORG || P_LOGO
                                  ELSE
                                    'M' || P_ORG || P_LOGO
                                  END;
V_RBS_CODE dtype.Name%type := 'MIGR';
V_ADD_PARAMS dtype.Name%type := 'REPL_MIN=' || P_REPL_CRD_MIN || ';' ||'REPL_MAX=' || P_REPL_CRD_MAX || ';';
V_PROD_CUSTOM_DATA dtype.Name%type := 'PURGE_DELAY_TYPE=M;PURGE_DELAY_VALUE=' || P_PURGE_MONTH || ';' || 'CLOSE_DELAY_TYPE=M;CLOSE_DELAY_VALUE=' || P_CLOSE_MONTH || ';'||'LTY_PROD_ENRL_STATUS='||P_LTY_PROD_ENRL_STATUS||';';
V_CHANNEL dtype.Name%type := CASE WHEN SUBSTR(P_BIN_PREFIX,1,1) = 4 THEN
                              'VISA'
                             ELSE
                              'Our MC Cards'
                             END;
FI_LEVEL  DTYPE.NAME%TYPE;
ErrMsg dtype. ErrorMessage %Type;
BEGIN
/* Configuring Card Contr Sub Type */

v_fi_id := OPT_ENBD_INSTANTIATION.GET_FI_ID(P_ORG);

OPT_ENBD_INSTANTIATION.CREATE_CARD_SUBTYPE(V_CONTR_TYPE_CODE,
                                           P_ORG,
                                           V_CCAT,
                                           CASE WHEN SUBSTR(P_BIN_PREFIX,1,1) = 4 THEN
                                             V_CONTR_SUBTYPE_NAME ||' ' || P_LOGO
                                           ELSE
                                              'MC '  || V_CONTR_SUBTYPE_NAME ||' ' || P_LOGO
                                           END,
                                           P_BIN_PREFIX,
                                           P_BIN_RNG_MIN,
                                           P_BIN_RNG_MAX,
                                           P_EXP_FOR_NEW,
                                           P_EXP_FOR_RENEW,
                                           V_SERVICE_CODE,
                                           V_PM_CODE,
                                           V_NUMERATION_TYPE,
                                           ' ',
                                           V_PLASTIC_CODE,
                                           V_RBS_CODE,
                                           V_ADD_PARAMS,
										   P_CHIP_SCHEME_NAME);

/*Check if Account Scheme exists*/

select count(*) into counter from acc_scheme acs where
  acs.f_i = v_fi_id and
  acs.scheme_name = P_ORG||'-'||P_CURR||'-Credit Card Standard' and
  acs.amnd_state = stnd.Active;
IF counter != 1 then
    stnd.process_message(stnd.error, 'Account Scheme does not exist or found more than one: ' ||P_ORG||'-'||P_CURR||'-Credit Card Standard');
  RETURN;
END IF; 

/*Check if Main Product Subtype exists*/

select count(*) into counter from contr_subtype cs where
  cs.f_i = v_fi_id and
  cs.name = P_ORG||'-Private Client Account' and
  cs.amnd_state = stnd.Active;
IF counter != 1 then
    stnd.process_message(stnd.error, 'Subtype does not exist or found more than one: ' ||P_ORG||'-Private Client Account');
  RETURN;
END IF;

/*Check if Main Product Serv Pack exists*/

select count(*) into counter from serv_pack sp where
  sp.f_i = v_fi_id and
  sp.name = P_ORG||'-Accounting Private Basic' and
  sp.amnd_state = stnd.Active;
IF counter != 1 then
    stnd.process_message(stnd.error, 'Service Pack does not exist or found more than one: '||P_ORG||'-Accounting Private Basic');
  RETURN;
END IF;                        

IF opt_util.branch_code_clearing (opt_util.get_fi_id(P_ORG)) = '100' then
     FI_LEVEL:= 'ENBD';
    ELSE 
     FI_LEVEL:= P_ORG || '_MAIN';
END IF;  

V_DOMAIN_PATH:= FI_LEVEL || '#' ||  P_ORG || '_'||P_CURR||'_'||P_LOGO;

TRF_AUX.CREATE_OR_UPDATE_TARIFF_DOMAIN(V_DOMAIN_PATH,
                                       P_ORG || '-TD '||P_CURR||' '||P_NAME,
                                       stnd.No,
                                       null,
                                       DomainID,
                                       RetCode,
                                       RetMsg);
stnd.process_message(stnd.Information, 'Tariff domain created for ' || P_ORG || '-TD '|| P_CURR || P_NAME ||'. Result Id=' || DomainID);			


OPT_ENBD_INSTANTIATION.CREATE_APPL_PRODUCT(P_ORG,
                                           NULL,
                                           P_ORG||'-'||UPPER(P_NAME),
                                           P_ORG||'_'||P_CURR||'_'||P_LOGO||'_A',
                                           P_CODE_2,
                                           'C',
                                           'P',
                                           'A',
                                           P_ORG||'-'||P_CURR||'-Credit Card Standard',
                                           P_ORG||'-Private Client Account',
                                           P_ORG||'-Accounting Private Basic',
                                           'Basic Date Scheme',
                                           'Main Account',
                                           'Issuing Credit',
                                           'Y',
                                           'Y',
                                           NULL,
                                           P_MAX_CR_LIM,
                                           '',
                                           NULL);  

ProdRec.id := OPT_NI_INST.GET_PRODUCT_BY_PATH(P_ORG||'_'||P_CURR||'_'||P_LOGO||'_A');
YGAPPL_PRODUCT(ProdRec.id, ProdRec);
ProdRec.MIN_CREDIT_LIMIT:= NVL(P_MIN_CR_LIM,0);
ProdRec.DEFAULT_CREDIT_LIMIT:=NVL(P_MIN_CR_LIM,0);
ProdRec.CUSTOM_DATA := V_PROD_CUSTOM_DATA;
YTAPPL_PRODUCT('P',ProdRec.id, ProdRec);   
YAPPL_PRODUCT(ProdRec.id,'E',ProdRec.id);

 stnd.process_message(stnd.information, 'Product created or updated: '||P_ORG||'_'||P_CURR||'_'||P_LOGO||'_A');

 /*Check if Card Product Subtype exists*/

select min(id) into ContrSubtype.id  from contr_subtype cs where
  cs.f_i = v_fi_id and
  cs.prefix = P_BIN_PREFIX and
  cs.max_number = P_BIN_RNG_MAX and
  cs.min_number = P_BIN_RNG_MIN and
  (cs.pm_code = V_PM_CODE or cs.pm_code is null) and
  cs.amnd_state = stnd.Active;
  YGCONTR_SUBTYPE (ContrSubtype.id, ContrSubtype);

IF ContrSubtype.id is null  then
    stnd.process_message(stnd.error, 'Subtype does not exist  for BIN ' ||P_BIN_PREFIX|| ' and PM_CODE ' || V_PM_CODE);
    RETURN;
END IF;


/* Validating the Card Contract Subtype */
ErrMsg:=CDBC.VALIDATE_SUBTYPE(ContrSubtype.id);

 /*Check if Card Product Serv Pack exists*/

ServPack.name := CASE WHEN SUBSTR(P_BIN_PREFIX,1,1) = 4 THEN
                              P_ORG || '-Our Priv VISA Basic'
                             ELSE
                              P_ORG || '-Our Priv EC/MC'
                  END;

ServPack := opt_enbd_instantiation.get_serv_pack(P_ORG,ServPack.name,XWCONTR_SUBTYPE('CONTR_TYPE__OID',ContrSubtype.contr_type__oid)); 

IF  ServPack.id  is null  then
      stnd.process_message(stnd.error, 'Service Pack does not exist  '||servpack.name);
      RETURN;
END IF;


OPT_ENBD_INSTANTIATION.CREATE_APPL_PRODUCT(P_ORG,
                                           P_ORG||'_'||P_CURR||'_'||P_LOGO||'_A',
                                           P_ORG||'-'||UPPER(P_NAME)||' Primary',
                                           P_ORG||'_'||P_CURR||'_'||P_LOGO||'_C_P',
                                           P_CODE_2,
                                           'C',
                                           'P',
                                           'C',
                                           P_ORG||'-'||P_CURR||'-Credit Card Standard',
                                           ContrSubtype.name,
                                           ServPack.name,
                                           'Basic Date Scheme',
                                           'Main Card',
                                           'Issuing Credit',
                                           'N',
                                           'Y',
                                           NULL,
                                           NULL,
                                           '',
                                           NULL);

stnd.process_message(stnd.information, 'Product created or updated: '||P_ORG||'_'||P_CURR||'_'||P_LOGO||'_C_P');

IF P_SUPPL_CRD_REQ=stnd.Yes then
   OPT_ENBD_INSTANTIATION.CREATE_APPL_PRODUCT(P_ORG,
                                              P_ORG||'_'||P_CURR||'_'||P_LOGO||'_A',
                                              P_ORG||'-'||UPPER(P_NAME)||' Supplementary',
                                              P_ORG||'_'||P_CURR||'_'||P_LOGO||'_C_S',
                                              P_CODE_2,
                                              'C',
                                              'P',
                                              'C',
                                              P_ORG||'-'||P_CURR||'-Credit Card Standard',
                                              ContrSubtype.name,
                                              ServPack.name,
                                              'Basic Date Scheme',
                                              'Supplementary Card',
                                              'Issuing Credit',
                                              'B',
                                              'Y',
                                              NULL,
                                              NULL,
                                              '',
                                              NULL);

  stnd.process_message(stnd.information, 'Product created or updated: '||P_ORG||'_'||P_CURR||'_'||P_LOGO||'_C_S');
END IF;

IF P_MMF_INDICATOR is not null THEN 

 V_CS_VALUE_CODE := SUBSTR(P_MMF_INDICATOR,instr(P_MMF_INDICATOR,'[',1)+1,LENGTH(P_MMF_INDICATOR)-instr(P_MMF_INDICATOR,'[',1)-1);

 OPT_ENBD_INSTANTIATION.CREATE_PRODUCT_OPTION_BYPATH(P_ORG||'_'||P_CURR||'_'||P_LOGO||'_A',
                                                     'MEMBERSHIP_FEE_INDICATOR',        
                                                     V_CS_VALUE_CODE,    
                                                     'N',                
                                                     NULL,             
                                                     NULL,               
                                                     NULL);

 OPT_ENBD_INSTANTIATION.CREATE_START_EVENT_BY_PRODPATH(P_ORG||'_'||P_CURR||'_'||P_LOGO||'_A',
                                                       'SET_MFI_' || V_CS_VALUE_CODE,
                                                       'N',
                                                       null,
                                                       null,
                                                       'Y',
                                                       0);													 
END IF;    

IF P_VAT_INT_CAL is not null THEN

 OPT_ENBD_INSTANTIATION.CREATE_PRODUCT_OPTION_BYPATH(P_ORG||'_'||P_CURR||'_'||P_LOGO||'_A',
                                                     'VAT_INT',        
                                                     UPPER(P_VAT_INT_CAL),    
                                                     'Y',                
                                                     NULL,             
                                                     NULL,               
                                                     NULL);
END IF;

IF P_DLQ_CNCL_PNTS is not null THEN 

V_CS_VALUE_CODE :=  nvl(ltrim(substr(P_DLQ_CNCL_PNTS,-2),'0'),'0');

OPT_ENBD_INSTANTIATION.CREATE_PRODUCT_OPTION_BYPATH(P_ORG||'_'||P_CURR||'_'||P_LOGO||'_A',
                                                    'DLQ_LEVEL_PNT_CANCEL',        
                                                    V_CS_VALUE_CODE,    
                                                    'Y',                
                                                    NULL,             
                                                    NULL,               
                                                    NULL);
END IF;                                                     

IF P_DLQ_EARN_PNTS is not null THEN 

 V_CS_VALUE_CODE :=  nvl(ltrim(substr(P_DLQ_EARN_PNTS,-2),'0'),'0');

 OPT_ENBD_INSTANTIATION.CREATE_PRODUCT_OPTION_BYPATH(P_ORG||'_'||P_CURR||'_'||P_LOGO||'_A',
                                                    'DLQ_LEVEL_PNT_EARN',        
                                                    V_CS_VALUE_CODE,    
                                                    'Y',                
                                                    NULL,             
                                                    NULL,               
                                                    NULL);
END IF; 

IF P_DLQ_LVL_PNT_REDEEM is not null THEN 

 V_CS_VALUE_CODE :=  nvl(ltrim(substr(P_DLQ_LVL_PNT_REDEEM,-2),'0'),'0');

 OPT_ENBD_INSTANTIATION.CREATE_PRODUCT_OPTION_BYPATH(P_ORG||'_'||P_CURR||'_'||P_LOGO||'_A',
                                                    'DLQ_LEVEL_PNT_REDEEM',        
                                                    V_CS_VALUE_CODE,    
                                                    'Y',                
                                                    NULL,             
                                                    NULL,               
                                                    NULL);
END IF; 

IF P_IS_FLAT_PLAN=stnd.Yes THEN
CONFIG_LIABLITY_PRODS(P_ORG,P_LOGO,P_CURR,'Y','Y');
END IF;

IF P_REF_PROD_BLCKCD IS NOT NULL THEN
 DUPLICATE_BC_HANDBOOK_PROD(P_ORG,P_REF_PROD_BLCKCD,P_LOGO);
 stnd.process_message(stnd.Information,'BLOCK CODE IS COPIED FROM'|| P_REF_PROD_BLCKCD|| 'TO' || P_LOGO);
END IF;

IF P_INCL_ACTRF ='Y' THEN
  OPT_EGCC_QUEST.CONFIG_PRODUCT_TRANSFER_OPTION(P_ORG,P_LOGO,CASE WHEN UPPER(P_INCL_PROD_LIST_ACTRF) = 'ALL' THEN 'Y' ELSE 'N' END,P_INCL_PROD_LIST_ACTRF);
END IF;

END CONFIG_ENBD_PRODUCT;

PROCEDURE OPT_EGCC_FLEX(P_ORG                   dtype.Name%type,
                        P_PRODUCT_CODE          dtype.Name%type,
                        P_PCT                   dtype.Name%type,
                        P_DEF_PCT               dtype.Name%type)
IS
V_FI F_I %ROWTYPE;
PROD_ID DTYPE.RECORDID%TYPE;
errMsg dtype.ErrorMessage %type;
prodRec appl_product%rowtype;
BEGIN
select min(id) into v_FI.id FROM F_I where Bank_Code = P_ORG and Amnd_state = 'A';
PROD_ID :=  opt_ni_inst.get_product_by_path(P_PRODUCT_CODE);

YGF_I(v_FI.id,V_FI);
IF GLOB.GET_TAG_VALUE(V_FI.SPECIAL_PARMS,'FLEX_AUTOCREATE_PCT') is null then
  V_FI.Special_parms := glob.SET_TAG_VALUE(V_FI.Special_parms,'FLEX_AUTOCREATE_PCT','E');
  YTF_I('P',V_FI.id,V_FI);
END IF;

opt_enbd_instantiation.inSERT_FLEX_LOGO_PCT(P_ORG,P_PRODUCT_CODE,P_PCT);
opt_enbd_instantiation.inSERT_FLEX_LOGO_DEF_PCT(P_ORG,P_PRODUCT_CODE,P_DEF_PCT);
ErrMsg := opt_flex_tools.Config_pct_by_product(PROD_ID);

prodRec.id := opt_ni_inst.get_product_by_path(P_PRODUCT_CODE);
YGAPPL_PRODUCT(prodRec.id,prodRec);
if opt_util.branch_code_clearing(prodRec.f_i) = '100' then
prodRec.tariff_domain := '';
end if;
YTAPPL_PRODUCT('P',prodRec.id,prodRec);
YAPPL_PRODUCT(prodRec.id,'E',prodRec.id);

END OPT_EGCC_FLEX;

PROCEDURE DUPLICATE_BC_HANDBOOK_PROD(P_ORG dtype.Name%type,
                                     FROM_LOGO dtype.Name%type,
                                     TO_LOGO DTYPE.Name%type)
IS
 rc dtype.Counter %type;
 hb SY_HANDBOOK%RowType;
 v_var dtype.LongStr%type;
BEGIN

 for hb in (select * from sy_handbook where amnd_state = 'A' and group_code = 'BLOCK_CODE_ACNT_STATUS' and filter = P_ORG and filter4 like P_ORG||'_%_'||FROM_LOGO||'_%')
 loop
     OPT_ENBD_INSTANTIATION.CREATE_HANDBOOK(p_group_code =>hb.GROUP_CODE,p_code=>hb.CODE,p_name=>hb.NAME,p_filter=>hb.FILTER,p_filter2=>hb.FILTER2,p_filter3=>hb.FILTER3,p_filter4=>P_ORG||'_%_'||TO_LOGO||'_%',p_filter5=>hb.FILTER5,p_int_filter=>hb.INT_FILTER,p_id_filter1=>hb.ID_FILTER1,p_id_filter2=>hb.ID_FILTER2,p_id_columns=>'FILTER2,FILTER3,FILTER4');
 end loop;
    stnd.process_message(stnd.Information,'Copied Block Code From '||FROM_LOGO||'to'||TO_LOGO);

FOR cs_action in (select * from cs_action where code like 'BLOCK_CODE_%_%_' || P_ORG and amnd_state='A') 
  LOOP
     FOR cs_action_rule in (select * from cs_action_rule where  amnd_state='A' and cs_action__oid=cs_action.id and glob.in_list(',', glob.get_tag_value(apply_rules, 'PR_CODE'), P_ORG||'_%_'||FROM_LOGO||'_%') = 1) 
       LOOP
       v_var:= glob.get_tag_value(cs_action_rule.apply_rules, 'PR_CODE');
       GLOB.ADD_LIST_ITEM(v_var,P_ORG||'_%_'||TO_LOGO||'_%',',');
       cs_action_rule.apply_rules := glob.set_tag_value(cs_action_rule.apply_rules, 'PR_CODE', v_var);
	   YTCS_ACTION_RULE('P',cs_Action_rule.id,cs_action_rule);
       stnd.process_message(stnd.Information,'Block code  '||P_ORG || '_%_' || TO_LOGO||'_%' ||'is added to apply rules field in cs_action_rule');
       END LOOP;
END LOOP;

END DUPLICATE_BC_HANDBOOK_PROD;

PROCEDURE CONFIG_INSTANTIATION_PARM(org        dtype.name%type,
                                    logo       dtype.name%type,
                                    parm_name  dtype.name%type,
                                    parm_type  dtype.name%type,
                                    parm_level dtype.name%type default NULL,
                                    parm_val   dtype.name%type,
									by_event   dtype.name%type,
                                    IS_EDITABLE   dtype.name%type default stnd.Yes,
									P_custom_rules Dtype.Name%TYPE default NULL,
                                    P_Stepn Dtype.Counter%TYPE default 0)
AS
fi_id dtype.RecordId%type;
Product appl_product%Rowtype;
ErrMsg dtype. ErrorMessage %Type;
v_by_event  dtype.Tag%type;
ProdStrtEvnt product_init_evnt%Rowtype;
BEGIN
fi_id := OPT_ENBD_INSTANTIATION.GET_FI_ID(org);  

v_by_event := CASE
               WHEN by_event = 'Product Option' THEN 'O'
               WHEN by_event = 'New Contract' THEN 'N'
               WHEN by_event = 'Lifecycle' THEN 'L'
               WHEN by_event = 'Close Contract' THEN 'C'
			   ELSE ' '
              END;

/* Fetch Contract Subtype Details */
IF (parm_type='Sub_Type_Tag' or parm_type='SubType_Valid') then
 FOR cst IN (SELECT *
             FROM contr_subtype
             WHERE amnd_state = 'A'
             AND f_i = fi_id
             AND SUBSTR(pm_code,1,3) = org
             AND SUBSTR(pm_code,-3,LENGTH(pm_code)) = logo
             AND ccat = 'P'
             AND con_cat = 'C')
   LOOP
/* Start Contract Subtype Tag Configuration */
IF parm_type='Sub_Type_Tag' then
   cst.add_parms := sy_convert.remove_tag(cst.add_parms,parm_name);
   IF parm_val is not null then
      cst.add_parms := glob.set_tag_value(cst.add_parms,parm_name, parm_val);
   ELSE
      cst.add_parms := glob.SET_TAG(cst.add_parms,parm_name);
   END IF;
   YTCONTR_SUBTYPE('P',cst.id,cst);
END IF;   
/* END Contract Subtype Tag Configuration */

/* Start Contract Subtype Validation Configuration */
IF parm_type='SubType_Valid' then
   cst.validation_type := sy_convert.remove_tag(cst.validation_type,parm_name);
   IF parm_val is not null THEN
      cst.validation_type := glob.set_tag_value(cst.validation_type,parm_name, parm_val);
   ELSE
      cst.validation_type := glob.SET_TAG(cst.validation_type,parm_name);
   END IF;
   YTCONTR_SUBTYPE('P',cst.id,cst);   
END IF;
/* END Contract Subtype Validation Configuration */
ErrMsg:=cdbc.VALIDATE_SUBTYPE(cst.id);
END LOOP;
END IF;

/* Fetch Product and Sub Product Details */
IF (parm_type='Prod_Code2' or parm_type='Prod_Tag' or parm_type='Prod_Option' or parm_type='Start_Event') THEN
FOR prod IN (SELECT main.id mainProdId,
                    main.code mainProd,
                    prim.id primSubProdId,
                    prim.code primSubProd,
                    supply.id supplSubProdId,
                    supply.code supplSubProd
             FROM appl_product main
             LEFT OUTER JOIN appl_product prim
             ON prim.appl_product__oid = main.id
             AND prim.ccat             = 'P'
             AND prim.con_cat          = 'C'
             AND prim.contract_role    = 'MAIN_CARD'
             AND prim.amnd_state       = 'A'
             LEFT OUTER JOIN appl_product supply
             ON supply.appl_product__oid = main.id
             AND supply.ccat = 'P'
             AND supply.con_cat = 'C'
             AND supply.contract_role = 'SUPPL_CARD'
             AND supply.amnd_state = 'A'
             WHERE main.amnd_state ='A'
             AND main.f_i = fi_id
             AND SUBSTR(main.code,1,3) = org
             AND SUBSTR(main.code,instr(main.code,'_', 1, 2)+1,3) = logo
             AND main.appl_product__oid IS NULL
             AND main.ccat = 'P'
             AND main.con_cat = 'A')
LOOP
/* Start Product Code2 Configuration */
IF parm_type='Prod_Code2' THEN
   IF parm_level='Main_Account'  THEN
      YGAPPL_PRODUCT(prod.mainProdId,Product);
      Product.code_2:=parm_val;
      YTAPPL_PRODUCT('P',Product.id,Product);
   IF parm_val ='SKY' THEN
      opt_enbd_instantiation.create_handbook(p_group_code => parm_name , p_code => 'SKYWARDS' ,p_filter => org, p_filter2=> 'TD_AUTH_SCH', p_filter3=> parm_val , p_name => 'Skywards Number',p_id_columns=>'CODE,FILTER,FILTER2,FILTER3');
   ELSIF parm_val='RTA' THEN
      opt_enbd_instantiation.create_handbook(p_group_code => parm_name , p_code => 'RTA_TAG_ID' ,p_filter => org, p_filter2=> 'TD_AUTH_SCH', p_filter3=> parm_val , p_name => 'RTA Tag',p_id_columns=>'CODE,FILTER,FILTER2,FILTER3');
      opt_enbd_instantiation.create_handbook(p_group_code => parm_name, p_code => 'NOL_ATU' ,p_filter => org, p_filter2=> 'APPL_INFO', p_filter3=>   parm_val , p_name => 'RTA Info: NOL Info',p_id_columns=>'CODE,FILTER,FILTER2,FILTER3');
   END IF;
 END IF;
END IF;
/* END Product Code2 Configuration */

/* Start Product Tag Configuration */
IF parm_type='Prod_Tag' THEN
   IF parm_level='Main_Account'  THEN
      YGAPPL_PRODUCT(prod.mainProdId,Product);
      Product.custom_data := sy_convert.remove_tag(Product.custom_data,parm_name);
      IF parm_val is not null then
         Product.custom_data := glob.set_tag_value(Product.custom_data,parm_name, parm_val);
      ELSE
         Product.custom_data := glob.SET_TAG(Product.custom_data,parm_name);
      END IF;
      YTAPPL_PRODUCT('P',Product.id,Product);
   END IF;
   IF parm_level='Main_Card'  THEN
      YGAPPL_PRODUCT(prod.primSubProdId,Product);
      Product.custom_data := sy_convert.remove_tag(Product.custom_data,parm_name);
      IF parm_val is not null THEN
         Product.custom_data := glob.set_tag_value(Product.custom_data,parm_name, parm_val);
      ELSE
         Product.custom_data := glob.SET_TAG(Product.custom_data,parm_name);
      END IF;
      YTAPPL_PRODUCT('P',Product.id,Product);
   END IF;

    IF parm_level='Supplementary_Card'  THEN
      YGAPPL_PRODUCT(prod.supplSubProdId,Product);
      Product.custom_data := sy_convert.remove_tag(Product.custom_data,parm_name);
      IF parm_val is not null THEN
         Product.custom_data := glob.set_tag_value(Product.custom_data,parm_name, parm_val);
      ELSE
         Product.custom_data := glob.SET_TAG(Product.custom_data,parm_name);
      END IF;
      YTAPPL_PRODUCT('P',Product.id,Product);
   END IF;
   IF parm_level='Both_Card'  THEN
      YGAPPL_PRODUCT(prod.primSubProdId,Product);
      Product.custom_data := sy_convert.remove_tag(Product.custom_data,parm_name);
      IF parm_val is not null THEN
         Product.custom_data := glob.set_tag_value(Product.custom_data,parm_name, parm_val);
      ELSE
         Product.custom_data := glob.SET_TAG(Product.custom_data,parm_name);
      END IF;
      YTAPPL_PRODUCT('P',Product.id,Product);

      YGAPPL_PRODUCT(prod.supplSubProdId,Product);
      Product.custom_data := sy_convert.remove_tag(Product.custom_data,parm_name);
      IF parm_val is not null THEN
         Product.custom_data := glob.set_tag_value(Product.custom_data,parm_name, parm_val);
      ELSE
         Product.custom_data := glob.SET_TAG(Product.custom_data,parm_name);
      END IF;
      YTAPPL_PRODUCT('P',Product.id,Product);
   END IF;
END IF;
/* END Product Tag Configuration */

/* Start Product Option Configuration */
IF parm_type='Prod_Option' THEN 
   IF parm_level='Main_Account' THEN
      OPT_ENBD_INSTANTIATION.CREATE_PRODUCT_OPTION_BYPATH(prod.mainProd,parm_name,parm_val,'Y');
	  IF parm_name = 'INS_BP_CANC_DLQ_LVL' THEN
	    OPT_ENBD_INSTANTIATION.CREATE_START_EVENT_BY_PRODPATH(prod.mainProd,
                                                              'INS_BP',
                                                              'O',
                                                              null,
                                                              null,
                                                              'Y',
                                                              0);

	  ELSIF parm_name = 'INS_CS_CANC_DLQ_LVL' THEN
	     OPT_ENBD_INSTANTIATION.CREATE_START_EVENT_BY_PRODPATH(prod.mainProd,
                                                               'INS_CS',
                                                               'O',
                                                               null,
                                                               null,
                                                               'Y',
                                                               0);


	  ELSIF parm_name = 'INS_CP_CANC_DLQ_LVL' THEN
	      OPT_ENBD_INSTANTIATION.CREATE_START_EVENT_BY_PRODPATH(prod.mainProd,
                                                                'INS_CP',
                                                                'O',
                                                                null,
                                                                null,
                                                                'Y',
                                                                0);


	  ELSIF parm_name = 'INS_BPP_CANC_DLQ_LVL' THEN
	      OPT_ENBD_INSTANTIATION.CREATE_START_EVENT_BY_PRODPATH(prod.mainProd,
                                                                'INS_BPP',
                                                                'O',
                                                                null,
                                                                null,
                                                                'Y',
                                                                0);

	  ELSE
	       OPT_ENBD_INSTANTIATION.CREATE_START_EVENT_BY_PRODPATH(prod.mainProd,
                                                                 'INS_LF',
                                                                 'O',
                                                                 null,
                                                                 null,
                                                                 'Y',
                                                                 0);

	  END IF;
   END IF;

   IF parm_level='Main_Card' THEN
      OPT_ENBD_INSTANTIATION.CREATE_PRODUCT_OPTION_BYPATH(prod.mainProd || '#' || prod.primSubProd,parm_name,parm_val,IS_EDITABLE);
   END IF;

   IF parm_level='Supplementary_Card' THEN
      OPT_ENBD_INSTANTIATION.CREATE_PRODUCT_OPTION_BYPATH(prod.mainProd || '#' || prod.supplSubProd,parm_name,parm_val,IS_EDITABLE);
   END IF;

   IF parm_level='Both_Card' THEN
      OPT_ENBD_INSTANTIATION.CREATE_PRODUCT_OPTION_BYPATH(prod.mainProd || '#' || prod.primSubProd,parm_name,parm_val,IS_EDITABLE );
      OPT_ENBD_INSTANTIATION.CREATE_PRODUCT_OPTION_BYPATH(prod.mainProd || '#' || prod.supplSubProd,parm_name,parm_val,IS_EDITABLE);
   END IF;
END IF;
/* END Product Option Configuration */

/* Start Product Start Event Configuration */
IF parm_type='Start_Event' and parm_val='Y' THEN 
   IF parm_level='Main_Account' THEN
      OPT_ENBD_INSTANTIATION.CREATE_START_EVENT_BY_PRODPATH(prod.mainProd,parm_name,v_by_event,P_custom_rules,Null,'Y',P_Stepn);
	  IF parm_name = 'ACC_CANCEL_WAIT_TO_CLOSE_PERIOD' THEN
        select min(id) into ProdStrtEvnt.id from product_init_evnt where appl_product__oid = prod.mainProdId and initial_event = parm_name and amnd_state = 'A';
	  	YGPRODUCT_INIT_EVNT(ProdStrtEvnt.id,ProdStrtEvnt);
		ProdStrtEvnt.custom_rules := 'AUTO_CLOSE_PERIOD=M;DURATION=6;';
		YTPRODUCT_INIT_EVNT('P',ProdStrtEvnt.id,ProdStrtEvnt);
	  END IF;
      IF parm_name = 'OVL_FEE' THEN
        select min(id) into ProdStrtEvnt.id from product_init_evnt where appl_product__oid = prod.mainProdId and initial_event = parm_name and amnd_state = 'A';
		YGPRODUCT_INIT_EVNT(ProdStrtEvnt.id,ProdStrtEvnt);
		ProdStrtEvnt.custom_rules := 'OVL_FEE_REPEAT_MODE=ONCE_AND_END_OF_BILLING;';
		YTPRODUCT_INIT_EVNT('P',ProdStrtEvnt.id,ProdStrtEvnt);
	  END IF;
   END IF;

   IF parm_level='Main_Card' THEN
      OPT_ENBD_INSTANTIATION.CREATE_START_EVENT_BY_PRODPATH(prod.mainProd || '#' || prod.primSubProd,parm_name,v_by_event,P_custom_rules,Null,'Y',P_Stepn);
   END IF;

   IF parm_level='Supplementary_Card' THEN
      OPT_ENBD_INSTANTIATION.CREATE_START_EVENT_BY_PRODPATH(prod.mainProd || '#' || prod.supplSubProd,parm_name,v_by_event,P_custom_rules,Null,'Y',P_Stepn);
   END IF;

   IF parm_level='Both_Card' THEN
      OPT_ENBD_INSTANTIATION.CREATE_START_EVENT_BY_PRODPATH(prod.mainProd || '#' || prod.primSubProd,parm_name,v_by_event,P_custom_rules,Null,'Y',P_Stepn);
      OPT_ENBD_INSTANTIATION.CREATE_START_EVENT_BY_PRODPATH(prod.mainProd || '#' || prod.supplSubProd,parm_name,v_by_event,P_custom_rules,Null,'Y',P_Stepn);
   END IF;
END IF;
/* END Product Start Event Configuration */
END LOOP;
END IF;
END CONFIG_INSTANTIATION_PARM;

PROCEDURE CONFIG_PRODUCT_TRANSFER_OPTION(p_org       dtype.name%type,
                                         from_logo   dtype.name%type,
                                         p_forAll    dtype.name%type DEFAULT 'Y',
                                         to_logo     dtype.LongStr%type DEFAULT NULL)
IS
fromProd appl_product%rowtype;
toProd   appl_product%rowtype;
v_curr   dtype.name%type;
BEGIN
 select XWF_I('LOCAL_CURRENCY',local_currency) into v_curr from f_i where bank_code = p_org and amnd_state = stnd.Active;
 fromProd.id := opt_ni_inst.get_product_by_path(p_org||'_'||v_curr||'_'||from_logo||'_A');
 YGAPPL_PRODUCT(fromProd.id,fromProd);
 FOR SyHndActtrf IN (select * from sy_handbook where group_code = 'ACCTTRF_PRODUCTS' and filter3= p_org and filter2 = fromProd.name)
  LOOP
    YSY_HANDBOOK(SyHndActtrf.id,'D',SyHndActtrf.id);
  END LOOP;
 IF to_logo IS NOT NULL THEN
 FOR i  IN (SELECT regexp_substr(to_logo,'[^,]+',1,level) to_logo FROM dual CONNECT BY regexp_substr(to_logo,'[^,]+',1,level) IS NOT NULL) 
   LOOP
    -- toProd.id := opt_ni_inst.get_product_by_path(p_org||'_'||v_curr||'_'||i.to_logo||'_A');
	select min(id) into toProd.id from appl_product where  appl_product__oid is null and pcat = 'C' and con_cat = 'A' and code = p_org||'_'||v_curr||'_'||i.to_logo||'_A' and amnd_state = stnd.Active;
	 if toProd.id is not null then
        YGAPPL_PRODUCT(toProd.id,toProd);
        -- Configuration for Account Transfer 
         opt_enbd_instantiation.add_product_transfer_option(fromProd,p_forAll,toProd.code);
        stnd.process_message(stnd.Information,'ACCOUNT_TRANSFER IS  DONE FROM '|| from_logo|| ' TO '|| i.to_logo);  
	 end if;
   END LOOP;
     ELSE
        opt_enbd_instantiation.add_product_transfer_option(fromProd,p_forAll);
        stnd.process_message(stnd.Information,'ACCOUNT_TRANSFER IS  DONE FROM '|| from_logo|| ' to All products');
     END IF;
 /* Configuration for Account Transfer Billing cycyle */
 FOR billingactrf IN (select level code,case when lpad(level,2,'0') = '01' THEN '(01 - Only for Staff)' ELSE lpad(level,2,'0') END name,fromProd.code prodcode,p_org org from dual connect by level <= 31)
   LOOP
       opt_enbd_instantiation.create_handbook(p_group_code => 'BILLING_DAY_ACCTTRF', p_code => billingactrf.code, p_name => billingactrf.name, p_filter => billingactrf.prodcode,p_filter2 => billingactrf.org, p_id_columns => 'CODE,FILTER,FILTER2');
   END LOOP;
END CONFIG_PRODUCT_TRANSFER_OPTION;

PROCEDURE CONFIG_LIABLITY_PRODS(P_ORG    dtype.Name%type,
                                P_LOGO   dtype.Name%type,
                                P_CURR   dtype.Name%type,
                                P_DAC    dtype.Tag%type default 'Y',
                                P_BT     dtype.Tag%type default 'Y')
IS
FiLevel       dtype.Name%Type;
DomainID      dtype.RecordID%Type;
RetCode       dtype.Counter%Type;
RetMsg        dtype.ErrorMessage%Type;
ProdRec       appl_product%rowtype;
BEGIN
IF opt_util.branch_code_clearing (opt_util.get_fi_id(P_ORG)) = '100' then
     FiLevel:= 'ENBD';
    ELSE 
     FiLevel:= P_ORG || '_MAIN';
END IF;  

IF P_DAC = 'Y' THEN
 /* DAC Flat plan Sub Product Configuration */
 OPT_ENBD_INSTANTIATION.CREATE_APPL_PRODUCT(P_ORG,
                                            P_ORG||'_'||P_CURR||'_'||P_LOGO||'_A',
                                            P_ORG||'-'||'DAC'||' Flat Plan',
                                            P_ORG||'_'||P_CURR||'_'||'000'||'_'||'DAC',
                                            NULL,
                                            'C',
                                            'P',
                                            'A',
                                            P_ORG||'-'||P_CURR||'-'||'Flat Plan',
                                            P_ORG||'-Private Client Account',
                                            P_ORG||'-DAC Flat Transfer',
                                            'Basic Date Scheme',
                                            'Flat Plan',
                                            'Issuing Credit',
                                            '',
                                            '',
                                            'EXTRA_LIM_NORM=C-FO-FP_DAC,C-LN-FP_DAC,C-FS-FP_DAC,C-INT-FP_DAC,B-LN-FP_DAC,B-FS-FP_DAC,B-INT-FP_DAC,B-FO-FP_DAC,B-INS-FP,B-FS-FP,C-INS-FP,C-FS-FP,B-VT-FP_DAC,C-VT-FP_DAC;',
                                            '',
                                            'Y',
                                            'DAC',
											'Y');
stnd.process_message(stnd.information, 'DAC Flat Plan Liability Sub Product created or updated: '|| P_ORG||'_'||P_CURR||'_'||'000'||'_'||'DAC');                                           
 /* DAC Flat plan Tariff Configuration */                                           
 TRF_AUX.CREATE_OR_UPDATE_TARIFF_DOMAIN(FiLevel || '#' ||  P_ORG || '_'|| 'DAC' ||'_FLAT_PLAN',--'ENBD#'||P_ORG||'_'||P_CURR||'_'||P_LOGO;
                                        P_ORG || '-TD '|| 'DAC'||' Flat Plan',
                                        stnd.No,
                                        null,
                                        DomainID,
                                        RetCode,
                                        RetMsg);

 stnd.process_message(stnd.Information, 'Tariff domain created for ' || P_ORG || '-TD '||'DAC'||' Flat Plan' ||'. Result Id=' || DomainID);										
 ProdRec.id := opt_ni_inst.get_product_by_path(P_ORG||'_'||P_CURR||'_'||P_LOGO||'_A'||'#'||P_ORG||'_'||P_CURR||'_'||'000'||'_'||'DAC');--033_AED_000_DAC
 YGAPPL_PRODUCT(ProdRec.id, ProdRec);    
 ProdRec.TARIFF_DOMAIN := DomainID;
 ProdRec.liab_category := 'Y';
 YTAPPL_PRODUCT('P',ProdRec.id, ProdRec);  
 YAPPL_PRODUCT(ProdRec.id,'E',ProdRec.id);  
END IF;

IF P_BT = 'Y' THEN
 /* BT Flat plan Type A Sub Product Configuration */
 OPT_ENBD_INSTANTIATION.CREATE_APPL_PRODUCT(P_ORG,
                                           P_ORG||'_'||P_CURR||'_'||P_LOGO||'_A',
                                           P_ORG||'-'||'BT'||' Flat Plan - Type A',
                                           P_ORG||'_'||P_CURR||'_'||'000'||'_'||'BT_A',
                                           NULL,
                                           'C',
                                           'P',
                                           'A',
                                           P_ORG||'-'||P_CURR||'-'||'Flat Plan',
                                           P_ORG||'-Private Client Account',
                                           P_ORG||'-BT Flat Transfer',
                                           'Basic Date Scheme',
                                           'Flat Plan',
                                           'Issuing Credit',
                                           '',
                                           '',
                                           'EXTRA_LIM_NORM=C-FO-FP_BT_A,C-LN-FP_BT_A,C-FS-FP_BT_A,C-INT-FP_BT_A,B-LN-FP_BT_A,B-FS-FP_BT_A,B-INT-FP_BT_A,B-FO-FP_BT_A,B-INS-FP,N-B-FS-FP,C-INS-FP,C-FS-FP,B-VT-FP_BT_A,C-VT-FP_BT_A;',
                                           '',
                                           'Y',
                                           'BT',
										   'Y');

 stnd.process_message(stnd.Information, 'BT Flat Plan Liability Sub Product created or updated: '|| P_ORG||'_'||P_CURR||'_'||'000'||'_'||'BT_A');
 /* BT Flat plan Type B Sub Product Configuration */
 OPT_ENBD_INSTANTIATION.CREATE_APPL_PRODUCT(P_ORG,
                                           P_ORG||'_'||P_CURR||'_'||P_LOGO||'_A',
                                           P_ORG||'-'||'BT'||' Flat Plan - Type B',
                                           P_ORG||'_'||P_CURR||'_'||'000'||'_'||'BT_B',
                                           NULL,
                                           'C',
                                           'P',
                                           'A',
                                           P_ORG||'-'||P_CURR||'-'||'Flat Plan',
                                           P_ORG||'-Private Client Account',
                                           P_ORG||'-BT Flat Transfer',
                                           'Basic Date Scheme',
                                           'Flat Plan',
                                           'Issuing Credit',
                                           '',
                                           '',
                                           'EXTRA_LIM_NORM=C-FO-FP_BT_B,C-LN-FP_BT_B,C-FS-FP_BT_B,C-INT-FP_BT_B,B-LN-FP_BT_B,B-FS-FP_BT_B,B-INT-FP_BT_B,B-FO-FP_BT_B,B-INS-FP,B-FS-FP,C-INS-FP,C-FS-FP,B-VT-FP_BT_B,C-VT-FP_BT_B;',
                                           '',
                                           'Y',
                                           'BT',
										   'Y');     

 stnd.process_message(stnd.Information, 'BT Flat Plan Liability Sub Product created or updated: '|| P_ORG||'_'||P_CURR||'_'||'000'||'_'||'BT_B'); 
 /* BT Flat plan Tariff Configuration */                                           
 TRF_AUX.CREATE_OR_UPDATE_TARIFF_DOMAIN(FiLevel || '#' ||   P_ORG || '_'|| 'BT' ||'_FLAT_PLAN',--'ENBD#'||P_ORG||'_'||P_CURR||'_'||P_LOGO;
                                        P_ORG || '-TD '|| 'BT'||' Flat Plan',
                                        stnd.No,
                                        null,
                                        DomainID,
                                        RetCode,
                                        RetMsg);

 ProdRec.id := opt_ni_inst.get_product_by_path(P_ORG||'_'||P_CURR||'_'||P_LOGO||'_A'||'#'||P_ORG||'_'||P_CURR||'_'||'000'||'_'||'BT_A');
 YGAPPL_PRODUCT(ProdRec.id, ProdRec);    
 ProdRec.TARIFF_DOMAIN := DomainID;
 ProdRec.liab_category := 'Y';
 YTAPPL_PRODUCT('P',ProdRec.id, ProdRec);
 YAPPL_PRODUCT(ProdRec.id,'E',ProdRec.id);
 ProdRec.id := opt_ni_inst.get_product_by_path(P_ORG||'_'||P_CURR||'_'||P_LOGO||'_A'||'#'||P_ORG||'_'||P_CURR||'_'||'000'||'_'||'BT_B');
 YGAPPL_PRODUCT(ProdRec.id, ProdRec);    
 ProdRec.TARIFF_DOMAIN := DomainID;
 ProdRec.liab_category := 'Y';
 YTAPPL_PRODUCT('P',ProdRec.id, ProdRec);
 YAPPL_PRODUCT(ProdRec.id,'E',ProdRec.id);

 stnd.process_message(stnd.Information, 'Tariff domain created for ' || P_ORG || '-TD '||'BT'||' Flat Plan' ||'. Result Id=' || DomainID);
END IF; 
END CONFIG_LIABLITY_PRODS;

procedure copy_tariff_with_data(p_from_domain_path   dtype.Name%type,
                                       p_to_domain_path     dtype.Name%type,
                                       p_with_data          dtype.Tag%type default stnd.No)

is
fromDomainId dtype.RecordID%type;
toDomainId   dtype.RecordID%type;
toTariff     tariff%rowtype;
toTariffData tariff_data%rowtype;
begin
fromDomainId := trf_aux.get_domain_id_by_path(p_from_domain_path);
toDomainId := trf_aux.get_domain_id_by_path(p_to_domain_path);
for toTariff in (select * from tariff where amnd_state = stnd.Active and tariff_domain__oid = toDomainId)
  loop
  for toTariffData in (select * from tariff_data where amnd_state = stnd.Active and tariff__oid = toTariff.id)
    loop
     YTARIFF_DATA(toTariffData.id,'D',toTariffData.id);
    end loop;
   YTARIFF(toTariff.id,'D',toTariff.id);
  end loop;

for fromTariff in (select * from tariff where amnd_state = stnd.Active and tariff_domain__oid = fromDomainId)
  loop
   YGTARIFF(fromTariff.id,toTariff);
   toTariff.id := null;
   toTariff.tariff_domain__oid := toDomainId;
   YTTARIFF('A',toTariff.id,toTariff);
   YTTARIFF('P',toTariff.id,toTariff);
   YTARIFF(toTariff.id,'E',toTariff.id);
   if p_with_data = stnd.Yes then
   for fromTariffData in (select max(id) id from tariff_data where amnd_state = stnd.Active and tariff__oid = fromTariff.id and is_ready = stnd.Yes group by is_active)
     loop
      YGTARIFF_DATA(fromTariffData.id,toTariffData);
      toTariffData.id := null;
      toTariffData.tariff__oid := toTariff.id;
      YTTARIFF_DATA('A',toTariffData.id,toTariffData);
      YTTARIFF_DATA('P',toTariffData.id,toTariffData);
      YTARIFF_DATA(toTariffData.id,'E',toTariffData.id);
     end loop;
    end if; 
  end loop;

end copy_tariff_with_data;

procedure copy_product_option(p_org       dtype.Name%type,
                              p_curr      dtype.Name%type,
                              p_ref_logo  dtype.Name%type,
                              p_logo      dtype.Name%type
)
is
prodRec appl_product%rowtype;
begin
prodRec.id := opt_ni_inst.get_product_by_path(p_org||'_'||p_curr||'_'||p_ref_logo||'_'||'A');

for prodOptn in (select replace(opt_ni_inst.get_product_path_by_id(c.appl_product__oid),p_ref_logo,p_logo) prodPath,
                        cst.code csTypeCode,
                        csv.code csValueCode,
                        c.editable,
                        c.custom_rules,
                        c.date_from,
                        c.date_to
                 from product_option c
                 left outer join cs_status_type cst on cst.id = c.option_id and cst.amnd_state = 'A'
                 left outer join cs_status_value csv on csv.id = c.default_value and csv.cs_status_type__oid = c.option_id
                 where appl_product__oid in
                   (select id from appl_product where id = prodRec.id AND amnd_state = 'A'
                   union
                   select id
                   from appl_product
                   where appl_product__oid = prodRec.id
                   and amnd_state = 'A' and (code not like '%_PP' and code not like '%INST%')
                   )
                 and c.amnd_state = 'A'
                 order by prodPath)

  loop
  opt_enbd_instantiation.create_product_option_bypath(prodOptn.prodPath,
                                                      prodOptn.csTypeCode,
                                                      case when prodOptn.csTypeCode='LOGO' then p_logo
                                                           else 
                                                           prodOptn.csValueCode
                                                           end,
                                                      prodOptn.editable,
                                                      prodOptn.custom_rules,
                                                      prodOptn.date_from,
                                                      prodOptn.date_to);


  end loop;

end copy_product_option;

procedure copy_start_events(p_org       dtype.Name%type,
                            p_curr      dtype.Name%type,
                            p_ref_logo  dtype.Name%type,
                            p_logo      dtype.Name%type,
                            p_excl_wshoper_card dtype.Tag%type default stnd.No
)
is
prodRec appl_product%rowtype;
begin
prodRec.id := opt_ni_inst.get_product_by_path(p_org||'_'||p_curr||'_'||p_ref_logo||'_'||'A');
for prdEvent in (select replace(opt_ni_inst.get_product_path_by_id(e.appl_product__oid),p_ref_logo,p_logo) prod_path,
                        e.initial_event init_event,
                        e.by_event byevnt,
                        e.custom_rules custrules,
                        trf_aux.domain_tree_code(e.tariff_domain,6) trfdom,
                        e.is_active actvie,
                        e.step_n stepn
                 from product_init_evnt e
                 where appl_product__oid in
                   (select id from appl_product where id = prodRec.id AND amnd_state = 'A'
                    union
                    select id
                    from appl_product
                    where appl_product__oid = prodRec.id
                    and amnd_state = 'A' and (code not like '%_PP' and code not like '%INST%')
                   )
                 and upper(xwproduct_init_evnt('INITIAL_EVENT',e.initial_event)) not like '%PCT%'
                 and e.amnd_state = 'A'
                 order by prod_path)
 loop
  if p_excl_wshoper_card='Y' and prdEvent.prod_path like  '%_C_W'   then
    stnd.process_message(stnd.information, 'eshopper card is not configured hence prod_path: '||prdEvent.prod_path||' is not found');
    else
 opt_enbd_instantiation.create_start_event_by_prodpath(prdEvent.prod_path,
                                                       prdEvent.init_event,
                                                       prdEvent.byevnt,
                                                       prdEvent.custrules,
                                                       prdEvent.trfdom,
                                                       prdEvent.actvie,
                                                       prdEvent.stepn);
end if;
 end loop;  

end copy_start_events;

function get_contract_sub_type(p_org  dtype.Name%type,
                               p_contr_subtype_name  dtype.Name%type)
return dtype.RecordId %type
is
 v_id dtype.RecordId %type;
begin
 select min(id) into v_id from CONTR_SUBTYPE where name = p_contr_subtype_name and f_i=opt_util.get_fi_id(p_org) and amnd_state = stnd.Active;
 if v_id is null then
 stnd.process_message(stnd.error, 'Contract Subype : '||'['|| p_contr_subtype_name ||'] is not found');
 end if;
 return v_id;

end get_contract_sub_type;

procedure DUPLICATE_PCT(p_org                     dtype.Name%type,
                        p_curr                    dtype.Name%type,
                        p_ref_logo                dtype.Name%type,
                        p_logo                    dtype.Name%type)
is
begin

for PCTs in (select substr(INITIAL_EVENT,-3) availPCT,
              (select substr(INITIAL_EVENT,-3) from PRODUCT_INIT_EVNT where amnd_State='A' and appl_product__oid=opt_ni_inst.get_product_by_path(p_org||'_'||p_curr||'_'||p_ref_logo||'_'||'A') and custom_rules='IS_PCT;' and BY_EVENT='N') defPCT
                from PRODUCT_INIT_EVNT where amnd_State='A' and appl_product__oid=opt_ni_inst.get_product_by_path(p_org||'_'||p_curr||'_'||p_ref_logo||'_'||'A') and custom_rules='IS_PCT;' and BY_EVENT='O' order by initial_event )
loop
OPT_EGCC_QUEST.OPT_EGCC_FLEX(p_org,p_org||'_'||p_curr||'_'||p_logo||'_'||'A',PCTs.availPCT,PCTs.defPCT);
end loop;

end DUPLICATE_PCT;



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
                                    )
 is
 fiId         dtype.RecordID%type;
 refProd      appl_product%rowtype;
 newProd      appl_product%rowtype;
 refSubProd   appl_product%rowtype;
 newSubProd   appl_product%rowtype;
 contrSubType contr_subtype%rowtype;
 domainPath   dtype.Name%type;
 mainDomain   dtype.Name%type;
 domainName   dtype.Name%type;
 tdPath  dtype.Name%type;
 refDomainPath tariff_domain%rowtype;
 newDomainApplyRules dtype.name%type;
 newDomainID  dtype.RecordID%type;
 retCode      dtype.Counter%type;
 retMsg       dtype.ErrorMessage%type;
 v_petra_check dtype.recordId%type;
 r_sp         serv_pack %rowtype;
 rsp_name     dtype.Name%type;
 Petra_orgs_list   DTYPE.LONGSTR%type := '002,008,013,018,030,050,055,061,301,995,996';

 begin
 fiId := opt_util.get_fi_id(p_org);

 /* Create Product level Tariff Domain */
 refProd.id := opt_ni_inst.get_product_by_path(p_org||'_'||p_curr||'_'||p_ref_logo||'_'||'A');
 YGAPPL_PRODUCT(refProd.id,refProd);
 domainPath := trf_aux.domain_tree_code(refProd.tariff_domain,6);
 v_petra_check:=glob.in_list(',',Petra_orgs_list,p_org);
 tdPath    :=    case when instr(domainPath,'#') > 0 then 
                                substr(domainPath,1,instr(domainPath,'#',1,1)-1)|| '#' || p_org || '_' || p_curr || '_' || p_logo
                       when  v_petra_check = 1 then
                                 '998_MAIN#'||p_org||'_MAIN#'|| p_logo 
                        else
                                  domainPath|| '#' || p_org || '_' || p_curr || '_' || p_logo
                   end;

 refDomainPath.id:=opt_trf_aux.GET_TD_ID_BY_PATH(replace(tdPath,p_logo,p_ref_logo));
 YGTARIFF_DOMAIN(refDomainPath.id,refDomainPath);
 newDomainApplyRules:=refDomainPath.apply_rules;

 domainName := case when p_new_trf_domain_name is not null then
                    p_new_trf_domain_name
                    else
                    p_org || '-' ||replace(p_name,substr(p_name,instr(p_name,'-')+1,instr(p_name,' ')-instr(p_name,'-')-1),'TD')
                    end;

 trf_aux.create_or_update_tariff_domain(tdPath,
                                        domainName,
                                        stnd.No,
                                        newDomainApplyRules,
                                        newDomainID,
                                        retCode,
                                        retMsg);

 stnd.process_message(stnd.information, 'Tariff Doamin created or updated: '|| tdPath);

 /* Copy Main Product Config */
 newProd                   := refProd;
 newProd.code              := replace(newProd.code,p_ref_logo,p_logo);
 newSubProd.main_product   := newProd.id;
 select min(id),min(internal_code) into newProd.id,newProd.internal_code from appl_product where amnd_state = stnd.Active and code = newProd.code;
 newProd.name              := p_org || '-' || p_name;
 newProd.code_2            := p_code2;
 newProd.max_credit_limit  := p_max_cr_lim;
 newProd.min_credit_limit  := p_min_cr_lim;
 newProd.default_credit_limit:= p_min_cr_lim;
 newProd.Tariff_Domain:=newDomainID;
 YTAPPL_PRODUCT('A',newProd.id,newProd);
 YTAPPL_PRODUCT('P',newProd.id,newProd);
 YAPPL_PRODUCT(newProd.id,'E',newProd.id);
 stnd.process_message(stnd.information, 'Product created or updated: '|| newProd.code);

/* Copy Sub Product Config */ 
 for refSubProd in (select * from appl_product where amnd_state = stnd.Active and f_i = fiId and appl_product__oid = refProd.id and (code not like '%_PP' and code not like '%INST%'))
     loop
    if refSubProd.code like '%_C_W' and p_exclude_Eshopper ='Y' then
      stnd.process_message(stnd.information, 'eshopper card is not configured');
    else
     YGAPPL_PRODUCT(refSubProd.id,refSubProd);
     newSubProd                   := refSubProd;
     newSubProd.code              := replace(refSubProd.code,p_ref_logo,p_logo);
     select min(id),min(internal_code) into newSubProd.id,newSubProd.internal_code from appl_product where amnd_state = stnd.Active and code = replace(refSubProd.code,p_ref_logo,p_logo) and appl_product__oid = newProd.id;
     if  newProd.name like '%PM%' then
     newSubProd.name              := p_org || '-' || 'PS' ||substr(p_name,3) 
											 || case when newSubProd.code like '%_P' then ' Primary'
                                                     when newSubProd.code like '%_S' then' Supplem' 
												  end;

	elsif opt_util.IS_ENBD(fiId)='Y' then
        newSubProd.name              := p_org || '-' || p_name
                                              ||case when newSubProd.code like '%_P' then ' PRIMARY'
                                                     when newSubProd.code like '%_S' then ' SUPPLEMENTARY' 
                                                     when newSubProd.code like '%_W' then ' ESHOPPER'
												  end;

         else                                         
	    newSubProd.name :=   case when newSubProd.name like '%INST%' then 
                                                upper(replace(p_name,substr(p_name,1,instr(p_name,' ',1,1)),substr(p_name,1,instr(p_name,' ',1,1)) || 'INST '))
                                               else
                                                upper(p_name)
                                               end || case when newSubProd.code like '%_PP' then ' PR PASS'
                                                  when newSubProd.code like '%_P' then ' PRIM'
                                                  when newSubProd.code like '%_S' then ' SUPP' 
                                             end;
		end if;									



 if newSubProd.name like '%INST%' then
        contrSubType.Id:=get_contract_sub_type(p_org,substr(p_contr_subtype_name,1,instr(p_contr_subtype_name,' ',1,1))||'INST'|| substr(p_contr_subtype_name,instr(p_contr_subtype_name,' ',1,1)));
     elsif newSubProd.name like '%REVOLVING PR PASS' then
        contrSubType.Id:=get_contract_sub_type(p_org,substr(p_contr_subtype_name,1,instr(p_contr_subtype_name,' ',1,2))||'PRIORITY PASS REVOL');
     elsif newSubProd.name like '%PR PASS' then
        contrSubType.Id:=get_contract_sub_type(p_org,substr(p_contr_subtype_name,1,instr(p_contr_subtype_name,' ',1,2))||'PRIORITY PASS');
		elsif newSubProd.name like '%ESHOPPER' then
         contrSubType.Id:=get_contract_sub_type(p_org,substr(p_contr_subtype_name,1,instr(p_contr_subtype_name,'-'))||xwappl_product('F_I',newSubProd.f_i)||' '||substr(p_contr_subtype_name,instr(p_contr_subtype_name,'-')+1)||' WS');
 else
        contrSubType.Id:=get_contract_sub_type(p_org,p_contr_subtype_name);
 end if;

 newSubProd.contr_subtype:=contrSubType.Id;
 YGCONTR_SUBTYPE(contrSubType.id,contrSubType);
 newSubProd.CONTR_TYPE:=contrSubType.CONTR_TYPE__OID;


  if newSubProd.LIAB_CATEGORY ='Y' then
      newSubProd.Name:=refSubProd.Name;
      newSubProd.CONTR_TYPE:=refSubProd.CONTR_TYPE;
      newSubProd.contr_subtype:=refSubProd.contr_subtype;
      r_sp:=opt_enbd_instantiation.get_serv_pack(p_org,xwappl_product('SERVICE_PACK',refSubProd.service_pack),xwappl_product('CONTR_TYPE',refSubProd.CONTR_TYPE));
  elsif  newSubProd.CONTR_TYPE <> refSubProd.CONTR_TYPE then 
     r_sp:= opt_enbd_instantiation.get_serv_pack(p_org,xwappl_product('SERVICE_PACK',refSubProd.service_pack),xwappl_product('CONTR_TYPE',refSubProd.CONTR_TYPE));
     if r_sp.name like '%MasterCard%' then
        rsp_name := replace(r_sp.name,'MasterCard','VISA');
     elsif  r_sp.name like '%VISA%' then
        rsp_name := replace(r_sp.name,'VISA','MasterCard');                                                          
     elsif  r_sp.name like '%Visa%' then
        rsp_name := replace(r_sp.name,'Visa','MC'); 
     elsif  r_sp.name like '%MC%' then
        rsp_name := replace(r_sp.name,'MC','Visa'); 
     end if;
    r_sp:= opt_enbd_instantiation.get_serv_pack(p_org,rsp_name,xwappl_product('CONTR_TYPE',newSubProd.CONTR_TYPE));
  else
     r_sp:= opt_enbd_instantiation.get_serv_pack(p_org,xwappl_product('SERVICE_PACK',newSubProd.service_pack),xwappl_product('CONTR_TYPE',newSubProd.CONTR_TYPE));
 end if; 


 newSubProd.service_pack := r_sp.id;
 newSubProd.appl_product__oid := newProd.id;
if  refSubProd.tariff_domain is not null and newSubProd.LIAB_CATEGORY ='Y' then
     newSubProd.Tariff_domain:=refSubProd.Tariff_domain;
elsif refSubProd.tariff_domain is not null then
     newSubProd.tariff_domain:=newDomainID;
 end if ;
 YTAPPL_PRODUCT('A',newSubProd.id,newSubProd);
 YTAPPL_PRODUCT('P',newSubProd.id,newSubProd);
 YAPPL_PRODUCT(newSubProd.id,'E',newSubProd.id);

 stnd.process_message(stnd.information, 'Sub Product created or updated: '|| newSubProd.code);
 end if;
end loop;                        


 /* Copy Product option */
 if p_dup_product_option = stnd.Yes then
    copy_product_option(p_org,p_curr,p_ref_logo,p_logo);
 end if;


 /* Copy Product Start Events */
 if p_dup_start_event = stnd.Yes and p_exclude_Eshopper = stnd.Yes then
    copy_start_events(p_org,p_curr,p_ref_logo,p_logo,p_exclude_Eshopper);
 elsif p_dup_start_event = stnd.Yes then
    copy_start_events(p_org,p_curr,p_ref_logo,p_logo);
 end if; 

 /* Copy PCT's */
 if p_dup_pct = stnd.Yes then
    duplicate_pct(p_org,p_curr,p_ref_logo,p_logo);
 end if; 

 /* Updating Correct Tariff Domain in Product */
 if domainPath is not null and instr(domainPath,'#') = 0 then
    newProd.tariff_domain  := refProd.tariff_domain;
 end if;
 YTAPPL_PRODUCT('A',newProd.id,newProd);
 YTAPPL_PRODUCT('P',newProd.id,newProd);
 YAPPL_PRODUCT(newProd.id,'E',newProd.id);

 /* Duplicate Block Code */
IF p_dup_block_code= stnd.Yes then
 DUPLICATE_BC_HANDBOOK_PROD(p_org,p_ref_logo,p_logo);
 stnd.process_message(stnd.Information,'BLOCK CODE IS COPIED FROM '|| p_ref_logo|| 'TO ' || p_logo);
END IF;


 /* Copy Tariff and Tariff Data*/
if p_dup_tariff = stnd.Yes then
    copy_tariff_with_data(replace(tdPath,p_logo,p_ref_logo),tdPath,p_dup_tariff_data);
    if p_dup_pct = stnd.Yes then
       for PctDomain in (select trf_aux.domain_tree_code(id,6) domPath from tariff_domain where amnd_state='A' and tariff_domain__oid =opt_trf_aux.GET_TD_ID_BY_PATH(replace(tdPath,p_logo,p_ref_logo)))
         loop
          copy_tariff_with_data(PctDomain.domPath,
		                       replace(substr(PctDomain.domPath,1,instr(PctDomain.domPath,'_',-1)-1),p_ref_logo,p_logo) || substr(PctDomain.domPath,-4),
							   p_dup_tariff_data);
         end loop;
    end if;
 end if; 

if p_incl_actrf ='Y' then
  if p_incl_prod_list_actrf='ALL' then
     OPT_EGCC_QUEST.CONFIG_PRODUCT_TRANSFER_OPTION(P_ORG,P_LOGO,CASE WHEN UPPER(p_incl_prod_list_actrf) = 'ALL' THEN 'Y' ELSE 'N' END);
  else
     OPT_EGCC_QUEST.CONFIG_PRODUCT_TRANSFER_OPTION(P_ORG,P_LOGO,CASE WHEN UPPER(p_incl_prod_list_actrf) = 'ALL' THEN 'Y' ELSE 'N' END,p_incl_prod_list_actrf);
  end if;
end if;

end copy_product_configuration;


END OPT_EGCC_QUEST;
