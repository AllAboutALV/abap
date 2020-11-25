REPORT zaaa_test_adoc_for_clas.

TYPES: BEGIN OF ts_adoc,
         line TYPE string,
       END OF ts_adoc,
       tt_adoc TYPE STANDARD TABLE OF ts_adoc.
DATA lt_adoc TYPE tt_adoc.
DATA gv_component TYPE seocompodf-cmpname.

PARAMETERS     p_clsnam TYPE seoclsname DEFAULT 'CL_GUI_ALV_GRID'.
SELECT-OPTIONS s_compo FOR gv_component .

INITIALIZATION.
  DATA(dock) = NEW cl_gui_docking_container( side = cl_gui_docking_container=>dock_at_bottom ratio = 70 ).
  DATA(text) = NEW cl_gui_textedit( parent = dock ).
  text->set_readonly_mode( 0 ).

AT SELECTION-SCREEN.

  CLEAR lt_adoc.
*  DATA(go_clas) = ycl_addict_class=>get_instance( p_clsnam ).
*
*  go_clas->get_components(
*    EXPORTING
*      param = VALUE #( cmpname_rng = CORRESPONDING #( s_compo[] ) )
*    IMPORTING
*      cmp_std = DATA(lt_components) ).

  DATA cls  TYPE REF TO cl_oo_class_components_flat.
  DATA itf  TYPE REF TO if_oo_clif_components_flat.
  DATA impl TYPE seor_implementing_r.
  DATA meth TYPE seometflat.
  DATA clskey TYPE seoclskey.

  clskey = p_clsnam.
  CREATE OBJECT cls
    EXPORTING
      clskey                      = clskey
      with_interface_components   = abap_true
      with_inherited_components   = abap_true
      with_enhancement_components = abap_false
    EXCEPTIONS
      not_existing                = 1
      is_interface                = 2
      deleted                     = 3
      model_only                  = 4
      OTHERS                      = 5.
  IF sy-subrc = 0.
    APPEND VALUE #( line = |= { clskey-clsname }\n | )  TO lt_adoc.

    TRY.
        itf ?= cls.
        LOOP AT itf->methods INTO meth WHERE exposure = 2 and cpdname in s_compo.
          APPEND VALUE #( line = |== { meth-cpdname } { meth-descript } \n| ) TO lt_adoc.

          APPEND VALUE #( line = |({
            SWITCH #( meth-exposure
              WHEN 0 THEN |Private|
              WHEN 1 THEN |Protected|
              WHEN 2 THEN |Public| ) } {
            SWITCH #( meth-mtdtype
              WHEN 0 THEN |Method|
              WHEN 1 THEN |Event handling method|
              WHEN 2 THEN |Constructor|
              WHEN 3 THEN |Destructor|
              WHEN 4 THEN |Get-method of a virtual attribute|
              WHEN 5 THEN |Set-method of a virtual attribute|
              WHEN 6 THEN |Test Method for ABAP Unit|
              WHEN 7 THEN |Method for CDS Table Function| ) }) \n| )  TO lt_adoc.

          IF meth-redefin = abap_true.
            APPEND VALUE #( line = 'redefined' ) TO lt_adoc.
          ENDIF.

          APPEND VALUE #( line = 'Interface' ) TO lt_adoc.
          APPEND VALUE #( line = '|========' ) TO lt_adoc.
          APPEND VALUE #( line = '|*Name*|*Typing*|*Description*' ) TO lt_adoc.


          LOOP AT itf->parameters INTO DATA(param)
          WHERE clsname = meth-clsname
            AND cpdname = meth-cpdname.

            APPEND VALUE #( line =
              |\|{ param-sconame } \|{ SWITCH #( param-typtype
                WHEN 0 THEN |LIKE|
                WHEN 1 THEN |TYPE|
                WHEN 2 THEN |TYPE|
                WHEN 3 THEN |TYPE REF TO|
                WHEN 4 THEN |see code|
                WHEN 5 THEN |TYPE ... BOXED| ) } { param-type } \|{ param-descript }| ) TO lt_adoc.
          ENDLOOP.
          APPEND VALUE #( line = '|========' ) TO lt_adoc.

          APPEND VALUE #( line = |\n| ) TO lt_adoc.
        ENDLOOP.

      CATCH cx_sy_move_cast_error.
    ENDTRY.

  ENDIF.

  DATA adoc TYPE string.
  LOOP AT lt_adoc INTO DATA(ls_adoc).
    adoc = |{ adoc }{ ls_adoc-line }\n|.
  ENDLOOP.

  text->set_textstream( adoc ).
  cl_gui_cfw=>flush( ).
