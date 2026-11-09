"""
Which SPICELIB routines go into which PEPPER module (used by
tools/modularize.py). Modules are listed in dependency order: a module's
routines may use only modules listed before it.

Steps to move the routines of a new module (MODULE = e.g. pepper_cells):

    python tools/modularize.py MODULE
    python tools/add_intents.py src/MODULE.f90
    pixi run python tools/mark_pure.py src/MODULE.f90
    python tools/document_procedures.py --remove-brief-io \
        src/MODULE.f90 src/legacy/pepper_legacy_<name>.f90
    python tools/modernize_syntax.py src
    python tools/wrap_docs.py src
    pixi run tspice

Steps for a context module (CONTEXT_MODULES; intents, documentation and
the Brief_I/O removal are done by modularize_ctx.py itself):

    python tools/data_to_parameter.py src/spicelib/<file>  (read-only tables)
    python tools/modularize_ctx.py --dry-run MODULE     (checks the calls)
    python tools/modularize_ctx.py MODULE
    python tools/modernize_syntax.py src
    python tools/tidy_comments.py src
    python tools/wrap_docs.py src
    pixi run tspice

For an umbrella moved with "state": "all", run tools/trim_state.py NAME to
turn its temporaries back into local variables, then list the remaining
state explicitly here and describe it in STATE_DOCS.

Constants computed on the first call (a saved FIRST flag) are rewritten as
parameters by hand before the move.
"""

# Constant functions of SPICELIB that are named constants in
# pepper_constants: name -> kind ("wp" real, "ip" integer).
CONSTANTS = {
    **{n: "wp" for n in "b1900 b1950 clight dpmax dpmin dpr halfpi j1900 j1950 "
                        "j2000 j2100 jyear pi rpd spd twopi tyear".split()},
    **{n: "ip" for n in "intmax intmin".split()},
}

MODULES = {
    "pepper_constants": sorted(CONSTANTS),
    "pepper_arrays": """
        alltru bschoc bschoi bsrchc bsrchd bsrchi clearc cleard cleari fillc
        filld filli isordv isrchc isrchd isrchi lstcld lstcli lstlec lstled
        lstlei lstltc lstltd lstlti maxac maxad maxai minac minad minai movec
        moved movei notru orderc orderd orderi prodad prodai reordc reordd
        reordi reordl rmdupc rmdupd rmdupi sameai shellc shelld shelli somfls
        somtru sumad sumai swapc swapd swapi
        touchc touchd touchi touchl
        """.split(),
    "pepper_math": """
        approx axisar azlrec brcktd brckti chbder chbint chbval cyllat cylrec
        cylsph dcbrt det drdcyl drdlat drdsph ducrss dvcrss dvdot dvhat dvnorm
        el2cgv even exact frame gcd ident insang invert latcyl latrec latsph
        mequ mequg mtxm mtxmg mtxv mtxvg mxm mxmg mxmt mxmtg mxv mxvg odd
        opsgnd opsgni pl2nvc pl2nvp pl2psv pltexp pltnrm polyds q2m qdq2av qxq
        radrec rav2xf recazl reccyl reclat recrad recsph sharpr smsgnd smsgni
        sphcyl sphlat sphrec trace traceg ucrss unorm unormg vadd vaddg vcrss
        vdist vdistg vdot vdotg vequ vequg vhat vhatg vhatip vlcom vlcom3
        vlcomg vminug vminus vnorm vnormg vpack vperp vproj vprojg vrel vrelg
        vrotv vscl vsclg vsclip vsep vsepg vsub vsubg vtmv vtmvg vupack vzero
        vzerog xf2rav xpose xposeg xpsgip zzmsxf zzrxr
        """.split(),
    "pepper_strings": """
        astrip bedec beint benum beuns cmprss cpos cposr eqstr esrchc fndnwd
        frstnb frstnp frstpc kxtrct lastnb lastpc lbuild lcase ljucrs ljust
        lparse lparsm ltrim lxqstr matchw nblen nbwid ncpos ncposr nextwd nthwd
        parsqs pcwid pos posr prefix quote replch replwd repmc rjust rtrim
        samch samsub scanrj shiftl shiftr sigdgt suffix ucase wdcnt wdindx
        zzinssub zzrepsub intstr dpstr eqchr nechr samchi samsbi matchi
        nparsd nparsi lx4uns
        """.split(),
    "pepper_errors": ["expln"],
}

# Routines that need the context (they signal errors): moved by
# tools/modularize_ctx.py into submodules of pepper_instance. In dependency
# order, like MODULES.
CONTEXT_MODULES = {
    "pepper_cells": """
        chbase enchar dechar excess
        cardc cardd cardi sizec sized sizei scardc scardd scardi
        ssizec ssized ssizei validc validd validi
        appndc appndd appndi copyc copyd copyi insrtc insrtd insrti
        removc removd removi elemc elemd elemi ordc ordd ordi setc setd seti
        unionc uniond unioni interc interd interi diffc diffd diffi
        sdiffc sdiffd sdiffi
        zzwninsd wncard wncomd wncond wndifd wnelmd wnexpd wnextd wnfetd
        wnfild wnfltd wnincd wninsd wnintd wnreld wnsumd wnunid wnvald
        """.split(),
    "pepper_numerics": """
        dacosh dacosn dasine datanh dazldr dcyldr dlatdr drdazl drdgeo dsphdr
        eqncpv edpnt ednmpt elltof hrmesp hrmint lgresp lgrind lgrint chbigr
        inelpl inrypl invort invstm isrot kepleq kpsolv m2q nplnpt npsgpt
        nvc2pl nvp2pl psv2pl pltar pltnp pltvol qderiv raxisa rquad rmaind
        rmaini vprjp vprjpi stelab stlabx xposbl
        """.split(),
    "pepper_array_ops": """
        cyacip cyadip cyaiip cyclac cyclad cyclai inslac inslad inslai
        remlac remlad remlai swapac swapad swapai packac packad packai
        lnkan lnkfsl lnkhl lnkila lnkilb lnkini lnknfn lnknxt lnkprv lnksiz
        lnktl lnkxsl
        """.split(),
    "pepper_text": """
        cyclec repmd repmi repsub remsub inssub shiftc chckid countc hx2int
        locati locln
        """.split(),
    "pepper_pool": """
        zzctr ioerr zzcln rdtext rdker zztknerr zzmkpc zzrept tcheck texpyr
        zztpats tpartv zztime tparse zzpini zzrvar zzrvbf zzgapool zzgpnm
        zznwpool pool
        """.split(),
    "pepper_bodies": """
        zzidmap zzhashi zzhsc zzhsi zzbodini zzbodker zzbodblt zzbodtrn
        bodn2c bodc2n bods2c bodc2s boddef bodfnd zzbods2c zzbodvcd bodvar
        bodvcd bodvrd
        """.split(),
    "pepper_files": """
        zzplatfm zzpltchk zzftpstr zzftpchk zzrbrkst zzddhini zzgetbff
        zzddhgsd zzddhmnm zzddhppf idw2at zzxlatei zzddhivf zzddhf2h zzddhrcm
        zzddhgtu zzddhrmu zzddhclu zzddhnfc zzddhman
        """.split(),
    "pepper_daf": """
        rdenci
        hx2dp
        rdencd
        txtopn
        txtopr
        wrenci
        int2hx
        dp2hx
        wrencd
        zzdafgfr zzdafnfr zzdafgdr zzdafgsr dafah dafrwa dafps dafrwd dafgda
        zzxlated dafrda dafrfr dafwfr dafwda dafarr dafrcr dafwcr dafra dafrrr dafac
        dafdc dafec dafana daffa dafbt dafa2b dafb2a dafb2t daft2b daftb
        """.split(),
    "pepper_das": """
        rdencc
        wrencc
        writln
        writla
        readln
        readla
        zzdasgrd zzdasgri zzdasnfr zzdasrfr dasfm dasrwr dasioc dasiod dasioi
        dasa2l dascud dasacr dasrcr dassdr dasadc dasadd dasadi dasrdc dasrdd
        dasrdi dasudc dasudd dasudi dascls dasrfr daswfr dasac dasacu
        dasdc dasec dasecu dasbt dastb daslla
        """.split(),
    "pepper_dsk_files": """
        zzsizeok
        zzckspk
        getfat
        surfnm
        nearpt
        recgeo
        zzvrtplt
        zzvoxcvo
        zzvox2id
        zztrvlnk
        zzuntngl
        zzinilnk
        zzgetvox
        zzaddlnk
        zzmkspin
        dlaopn dlabns dlaens dlabfs dlabbs dlafns dlafps dlassg dskopn dskcls
        dsktol dskgd dskb02 dskd02 dski02 dskn02 dskp02 dskv02 dskz02 dskmi2
        dskrb2 dskobj dsksrf
        """.split(),
    "pepper_segments": """
        spkbsr pckbsr ckbsr zzdskbsr
        """.split(),
    "pepper_time": """
        badkpv prsint convrt zzcvpool zzutcpm zzvstrng dpfmt dpstrf repmf jul2gr
        etcal ttrans timdef unitim str2et zzprscor zzvalcor zzcorepc
        """.split(),
    "pepper_sclk": """
        sclu01 zzscin01 zzscad01 zzscup01 zzsclk sc01
        """.split(),
    "pepper_rotations": """
        rotate rotmat m2eul eul2m xf2eul twovec zztwovxf chgirf zzmobliq zzwahr
        zzenut80 zzeprc76 zzdnpt dnearp zzstelab
        """.split(),
    "pepper_frames": """
        zzdynbid zzdynvai zzdynoac zzdynoad zzdynvac zzdynvad zzswfcln zzswfini zzgftreb zzfdat
        framex
        """.split(),
    "pepper_records": """
        cke01 cke02 cke03 cke04 cke05 ckr01 ckr02 ckr03 ckr05 ckr06
        pckr02 pckr20 sgmeta spke01 spke02 spke03 spke08 spke09 spke12 spke13
        spke14 spke17 spke18 spke19 spke20 spke21 spkr01 spkr02 spkr03 spkr05
        spkr08 spkr09 spkr15 spkr17 spkr18 spkr19 spkr20 spkr21 stmp03 zzbodbry
        zzck4d2i zzdscm zzdsin zzdspc zzdspr zzinil cke06 ckmeta cknr04 pcke02
        pcke20 prop2b sgfcon sgfpkt sgfrvi spkr12 spkr13 ckr04 pcke03 pckr03
        spke05 spke15 spkr10 spkr14 ckpfs pckmat zzsgp4
        """.split(),
    "pepper_orientation": """
        sctype tisbod tkfram zzdynfid zznamfrm zznofcon zzswffet zzteme sce2c spke10
        tipbod ckfrot ckfxfm spkpvn zzswfxfm
        """.split(),
    "pepper_dynamic": """
        zzfrmgt1 zzrotgt1 zzfrmch1 zzrefch1 zzspkgo1 zzspkgp1 zzspklt1 zzspkpa1 zzspksb1 zzspkas1
        zzspkzp1 zzspkac1 zzspkez1 zzdynfr0 zzfrmgt0 zzfrmch0 zzspkgo0 zzspklt0 zzspksb0 zzspkas0
        zzspkac0 zzspkez0 zzdynrt0 zzrotgt0 zzrefch0 zzspkgp0 zzspkpa0 zzspkzp0 zzdynfrm zzdynrot
        """.split(),
    "pepper_transforms": """
        frmget rotget frmchg refchg pxform sxform
        """.split(),
    "pepper_time_api": """
        deltet et2utc scanit tpictr utc2et timout
        """.split(),
    "pepper_sclk_api": """
        sce2t scfmt scpart sct2e sctiks scdecd scencd sce2s scs2e
        """.split(),
    "pepper_ck_api": """
        ckobj ckgp ckgpav zzckcv01 zzckcv02 zzckcv03 zzckcv04 zzckcv05 zzckcv06 ckcov
        """.split(),
    "pepper_pck_api": """
        pckcov pckeul pckfrm bodeul
        """.split(),
    "pepper_dsk_write": """
        dskw02
        """.split(),
    "pepper_spk_api": """
        sgfref sgseqw spkcov spkgeo spkgps spkobj spkpds spks01 spks02 spks03
        spks05 spks08 spks09 spks15 spks17 spks18 spks19 spks20 spks21 spkuds
        zzcorsxf zzcvstat spk14a spk14b spk14e spkapo spkltc spks10 spks12 spks13
        spkssb spkapp spkaps spkezp spks14 zzspkflt spkacs spkpos spksub zzspkfao
        zzspkfap spkez zzspkfat zzspkfzo spkcvo spkezr zzspkfzt spkcpo spkcvt spkcpt
        """.split(),
    "pepper_keeper": """
        inttxt intord repmot sepool stpool tkvrsn zzascii exists zzldker keeper
        """.split(),
    "pepper_symbols": """
        sydelc sydeld sydeli sydimc sydimd sydimi sydupc sydupd sydupi syfetc
        syfetd syfeti sygetc sygetd sygeti synthc synthd synthi syordc syordd
        syordi sypopc sypopd sypopi syputc syputd syputi syselc syseld syseli
        sysetc sysetd syseti sytrnc sytrnd sytrni syenqc syenqd syenqi sypshc
        sypshd sypshi syrenc syrend syreni
        """.split(),
    "pepper_writers": """
        ckcls ckgr01 ckgr03 ckgr04 ckgr05 ckgr06 ckmp06 cknm06 cknr01 cknr02
        cknr03 cknr05 ckopn ckw01 ckw02 ckw03 ckw04b ckw04e ckw05 ckw06
        pck03a pck03e pckcls pckopn pckpds pckuds pckw02 pckw20 spcac spcdc
        spcec spcopn spcrfl spkcls spkopa spkopn spkw01 spkw02 spkw03 spkw05
        spkw08 spkw09 spkw10 spkw12 spkw13 spkw15 spkw17 spkw18 spkw19 spkw20
        spkw21 zzck4i2d ckgr02 ckw04a pck03b spcb2t spct2b spca2b spcb2a
        """.split(),
    "pepper_geometry": """
        gfstep gfrefn
        ana azlcpo bltfrm bodmat conics delfil dgeodr dhfa diags2 drotat
        dxtrct ev2lin evsgp4 fetchc fetchd fetchi fn2lun georec getfov hyptof
        illum irftrn iso2utc isopen kplfrm lparss lspcn ltime lun2fn lx4sgn
        lxname npelpt oscelt partof plnsns prompt prsdp prtenc pxfrm2 rdnbl
        repmct repml rotvec scps01 sctran sphsd spkpv srfrec stdio surfpt
        twovxf uddf udf zzasryel zzbquad zzchrlat zzcputim zzcxbrut zzdiv zzdskbbl
        zzdsksbi zzdsksbr zzdsksel zzdsksgr zzedterm zzedtmpt zzellbds zzelnaxx zzeprcss zzgetelm
        zzgrav zzholdd zzhullax zzingrd zzinrec zzinrypl zzlatbox zzlexmet zzmult zznrmlon
        zzrecbox zzrtnmat zzryxsph zzsecprt zzsepq zzsfxcor zzsglatx zzspin zzspkap0 zzspkap1
        zzsrfini zztanslv zztogrid zzwind zzwind2d dpgrdr drdpgr dvsep edterm getelm
        getfvn lx4dec oscltx pgrrec recpgr saelgv scpars subpt subsol surfpv
        tostdo trgsep uddc wrkvar xdda zzcnquad zzdsksph zzfovaxi zzinlat0 zznrddp
        zzpdcmpl zzpdpltc zzpdtbox zzraybox zzsrfker cgv2el dpspce et2lst incnsg lx4num
        rdkvar xfmsta zzilusta zzinlat zzinpdt0 zzrytrec zzsegbox zzsrftrn inedpl pjelpl
        srfc2s srfcss srfs2c srfscc zzdsksba zzelvupy zzinpdt zzrytlat edlimb npedln
        zzinvelt zzprsmet zzrytpdt dskx02 srfxpt tangpt zzocced zzptpl02 zzrytelt zzdskbun
        zzdsksgx zzdskbux zzdsksbf dskxsi dskxv illumg srfnrm zzsinutl illumf ilumin
        latsrf sincpt subpnt subslr zztanutl zztangnt limbpt termpt
        """.split(),
    "pepper_gf": """
        gfbail gfstol zzgfcoq zzgfcprx zzgfdiq zzgfdsps zzgffvu zzgfilu zzgfocu zzgfpaq
        zzgfref zzgfrrq zzgfsolv zzgfsolvx zzgfspq zzgfspu zzgfssin zzgfssob zzgfudlt zzgfwsts
        fovray fovtrg gffove gfocce occult phaseq zzgfcost zzgfdiu zzgfpau zzgfrel
        zzgfrelx zzgfrpwk zzgfrru zzgfudb gfoclt gfrfov gfrprt gftfov gfudb gfuds
        zzgfcou zzgflong zzgfcslv gfevnt gfdist gfilum gfpa gfposc gfrr gfsep
        gfsntc gfsubc
        """.split(),
    "pepper_ek_storage": """
        ekcls ekshdw zzekcdsc zzekcnam zzekgei zzekgrs zzekordc zzekordd zzekordi zzekpdec
        zzekpgch zzekqini zzekrbck zzekreqi zzeksca zzekscan zzeksei zzekstyp zzeksz04 zzeksz05
        zzektcnv zzektloc zzekweqi ekopr ekopw zzekgrcp zzekinqc zzekinqn zzekjsqz zzekpage
        zzekqcnj zzekqcon zzekqord zzekqsel zzekqtab zzekrd09 zzeksrcp zzeksrs zzeksz06 zzekvadr
        zzekgfwd zzekglnk zzeknrml zzeksfwd zzekslnk zzektr13 zzektr31 zzektrbs zzektrfr zzektrit
        zzektrlk zzektrpi zzektrrk zzekweed ekopn ekops zzekcix1 zzekpars zzektr23 zzektr32
        zzektrdp zzektrnk zzektrsb zzektrsz zzektrud zzektrui eknseg zzekixlk zzekrplk zzektr1s
        zzektrbn zzektrki zzektrls zzekmloc zzektrdl zzektrin zzekdps zzekff01 zzeksdsc zzeksinf
        zzektrap ekffld ekssum zzekacps zzekaps zzekbs01 zzekbs02 zzekde04 zzekrp2n
        """.split(),
    "pepper_ek": """
        ekbseg ekinsr stcc01 zzekac01 zzekac02 zzekac03 zzekac04 zzekac05 zzekac06 zzekad04
        zzekad05 zzekad06 zzekde05 zzekde06 zzekesiz zzekgcdp zzekif01 zzekif02 zzekrd01 zzekrd02
        zzekrd03 zzekrd04 zzekrd05 zzekrd06 zzekrd07 zzekrd08 zzekscdp zzekwpac zzekwpai zzekwpal
        ekappr ekifld ekrcec ekrced ekrcei zzekac07 zzekac08 zzekac09 zzekrsc zzekrsd
        zzekrsi zzekue04 zzekue05 zzekue06 ekaclc ekacld ekacli zzekecmp zzekscmp zzekerc1
        zzekerd1 zzekeri1 zzekllec zzeklled zzekllei zzeklltc zzeklltd zzekllti zzekrcmp zzekrmch
        zzekvcmp zzekvmch zzekille zzekillt zzekjsrt zzeklerc zzeklerd zzekleri zzekfrx zzekiic1
        zzekiid1 zzekiii1 zzekjtst zzekkey zzekad01 zzekad02 zzekad03 zzekixdl zzekjoin ekacec
        ekaced ekacei ekqmgr zzekde01 zzekde02 zzekde03 zzekue01 zzekue02 ekdelr ekuced
        ekucei stcg01 stcl01 zzekcchk zzekpcol zzeksemc zzektres zzekue03 ekucec zzeknres
        ekfind zzekencd ekpsel stcf01
        """.split(),
}

# Hand-written stateless modules (not generated from SPICELIB files) whose
# procedures moved code may call.
HANDWRITTEN = {
    "pepper_hash": ["name_hash"],
}

# Umbrella routines (shared saved variables and ENTRY points) moved by
# tools/modularize_ctx.py: each entry point becomes a module procedure. The
# "state" variables are the saved variables that really persist between
# calls; they become the components of a derived type (in a generated module
# pepper_state_<name>) held in pepper_spice as the allocatable component
# "component" ("where": "context" holds it in pepper_context instead, for
# state of a computation in progress). "state": "all" makes every local
# variable state (exactly SPICELIB's behavior with a bare SAVE). All other
# saved variables are temporaries and become plain local variables. An
# umbrella is listed by its name in CONTEXT_MODULES. "state": "saved" takes the
# variables named in SAVE statements or initialized by DATA; it also works for
# routines without ENTRY points (e.g. with a saved cache).
UMBRELLAS = {
    # Phase 6: EK. The query manager's tables of loaded EK files, tables,
    # segments and columns belong to the instance (they change only when EK files
    # are loaded or unloaded); a query (its selection, constraints, matching rows,
    # the scratch area) belongs to the context that runs it.
    "zzekpdec": {"state": "saved", "component": "zzekpdec_state", "where": "context", "entries": ['zzekpdec']},
    "zzekreqi": {"state": "saved", "component": "zzekreqi_state", "where": "context", "entries": ['zzekreqi']},
    "zzeksca": {"state": "saved", "component": "zzeksca_state", "where": "context", "entries": ['zzekstop', 'zzekspsh', 'zzekspop', 'zzeksdec', 'zzeksupd', 'zzeksrd', 'zzekscln']},
    "zzekscan": {"state": "saved", "component": "zzekscan_state", "where": "context", "entries": ['zzekscan']},
    "zzekweqi": {"state": "saved", "component": "zzekweqi_state", "where": "context", "entries": ['zzekweqi']},
    "zzekpage": {"state": "saved", "component": "zzekpage_state", "where": "context", "entries": ['zzekpgin', 'zzekpgan', 'zzekpgal', 'zzekpgfr', 'zzekpgrc', 'zzekpgrd', 'zzekpgri', 'zzekpgwc', 'zzekpgwd', 'zzekpgwi', 'zzekpgbs', 'zzekpgpg', 'zzekpgst']},
    "zzekvadr": {"state": "saved", "component": "zzekvadr_state", "where": "context", "entries": ['zzekvset', 'zzekvcal']},
    "zzeknrml": {"state": "saved", "component": "zzeknrml_state", "where": "context", "entries": ['zzeknrml']},
    "zzektrit": {"state": "saved", "component": "zzektrit_state", "where": "context", "entries": ['zzektrit']},
    "zzektrlk": {"state": "saved", "component": "zzektrlk_state", "where": "context", "entries": ['zzektrlk']},
    "ekbseg": {"state": "saved", "component": "ekbseg_state", "where": "context", "entries": ['ekbseg']},
    "stcc01": {"state": "saved", "component": "stcc01_state", "where": "context", "entries": ['stcc01']},
    "zzekad06": {"state": "saved", "component": "zzekad06_state", "where": "context", "entries": ['zzekad06']},
    "zzekjsrt": {"state": "saved", "component": "zzekjsrt_state", "where": "context", "entries": ['zzekjsrt']},
    "zzekjtst": {"state": "saved", "component": "zzekjtst_state", "where": "context", "entries": ['zzekjprp', 'zzekjnxt']},
    "ekqmgr": {"state": ['ftpool', 'fthan', 'fthead', 'tbpool', 'tbstpt', 'tbncol', 'tbnams', 'tbctpt', 'tbfils', 'tbflsz', 'tbhead', 'stpool', 'sthan', 'stsidx', 'stdscs', 'stnrow', 'stncol', 'stdtpt', 'dtpool', 'dtdscs', 'ctpool', 'ctnams', 'ctclas', 'cttyps', 'ctlens', 'ctfixd', 'ctsizs', 'ctindx', 'ctnull', 'first'],
               "component": "ekqmgr",
               "context_state": ['cnams', 'cnmset', 'colnam', 'frmals', 'frmtab', 'lcname', 'ltname', 'problm', 'rcname', 'rtname', 'state', 'tabnam', 'tabvec', 'dvals', 'begidx', 'cbegs', 'cdscrs', 'cends', 'cjbeg', 'cjend', 'cjrows', 'cjsize', 'cnstyp', 'col', 'colptr', 'conj', 'cvlen', 'ctnew', 'delseg', 'dtnew', 'dtype', 'endidx', 'i', 'ivals', 'j', 'jbase1', 'jbase2', 'jsize', 'k', 'key', 'keydsc', 'l', 'lcidx', 'lelts', 'ldscrs', 'ltbidx', 'lxbeg', 'lxend', 'nact', 'ncols', 'nconj', 'new', 'next', 'nmatch', 'norder', 'npcol', 'nseg', 'nsel', 'nsv', 'ntab', 'ocols', 'oelts', 'ops', 'ordbas', 'otabs', 'ptroff', 'r', 'rbas', 'rcidx', 'rdscrs', 'relts', 'resbas', 'rowidx', 'rowvec', 'rsize', 'rtbidx', 'rtotal', 'rwvbas', 'selcol', 'selctp', 'seltab', 'seg', 'segdsc', 'segvec', 'sense', 'sgvbas', 'sizes', 'stnew', 't', 'tab', 'tabidx', 'tbcurr', 'top', 'tptvec', 'ubase', 'unrows', 'usize', 'activc', 'activv', 'attmch', 'cmtch', 'dosort', 'fnd', 'indexd', 'keyfnd', 'nulsok', 'presnt', 'sorted', 'vmtch'],
               "context_component": "ekqmgr_query",
               "entries": ['eklef', 'ekuef', 'ekntab', 'ektnam', 'ekccnt', 'ekcii', 'eksrch', 'eknelt', 'ekgc', 'ekgd', 'ekgi']},
    "zzeksemc": {"state": "saved", "component": "zzeksemc_state", "where": "context", "entries": ['zzeksemc']},
    # Phase 6: GF (geometry finder). The state of a search (the quantity, the
    # targets and corrections set by the *IN entry points, the progress report)
    # belongs to the context that runs the search. The callbacks take the context
    # (CALLBACKS).
    "zzgfcoq": {"state": "saved", "component": "zzgfcoq_state", "where": "context", "entries": ['zzgfcoq']},
    "zzgffvu": {"state": "saved", "component": "zzgffvu_state", "where": "context", "entries": ['zzgffvin', 'zzgffvst']},
    "zzgfilu": {"state": "saved", "component": "zzgfilu_state", "where": "context", "entries": ['zzgfilin', 'zzgfildc', 'zzgfilgq']},
    "zzgfocu": {"state": "saved", "component": "zzgfocu_state", "where": "context", "entries": ['zzgfocin', 'zzgfocst']},
    "zzgfspu": {"state": "saved", "component": "zzgfspu_state", "where": "context", "entries": ['zzgfspin', 'zzgfspdc', 'zzgfspgq', 'zzgfspx']},
    "zzgfssin": {"state": "saved", "component": "zzgfssin_state", "where": "context", "entries": ['zzgfssin']},
    "zzgfssob": {"state": "saved", "component": "zzgfssob_state", "where": "context", "entries": ['zzgfssob']},
    "fovtrg": {"state": "saved", "component": "fovtrg_state", "where": "context", "entries": ['fovtrg']},
    "phaseq": {"state": "saved", "component": "phaseq_state", "where": "context", "entries": ['phaseq']},
    "zzgfdiu": {"state": "saved", "component": "zzgfdiu_state", "where": "context", "entries": ['zzgfdiin', 'zzgfdidc', 'zzgfdigq']},
    "zzgfpau": {"state": "saved", "component": "zzgfpau_state", "where": "context", "entries": ['zzgfpain', 'zzgfpadc', 'zzgfpagq', 'zzgfpax']},
    "zzgfrel": {"state": "saved", "component": "zzgfrel_state", "where": "context", "entries": ['zzgfrel']},
    "zzgfrelx": {"state": "saved", "component": "zzgfrelx_state", "where": "context", "entries": ['zzgfrelx']},
    "zzgfrpwk": {"state": "saved", "component": "zzgfrpwk_state", "where": "context", "entries": ['zzgftswk', 'zzgfwkin', 'zzgfwkad', 'zzgfwkun', 'zzgfwkmo']},
    "zzgfrru": {"state": "saved", "component": "zzgfrru_state", "where": "context", "entries": ['zzgfrrin', 'zzgfrrdc', 'zzgfrrgq', 'zzgfrrx']},
    "gfrprt": {"state": "saved", "component": "gfrprt_state", "where": "context", "entries": ['gfrepi', 'gfrepu', 'gfrepf']},
    "gftfov": {"state": "saved", "component": "gftfov_state", "where": "context", "entries": ['gftfov']},
    "zzgfcou": {"state": "saved", "component": "zzgfcou_state", "where": "context", "entries": ['zzgfcoin', 'zzgfcog', 'zzgfcodc', 'zzgfcoex', 'zzgfcocg', 'zzgfcosg', 'zzgfcocd', 'zzgfcosd']},
    "zzgflong": {"state": "saved", "component": "zzgflong_state", "where": "context", "entries": ['zzgflong']},
    "zzgfcslv": {"state": "saved", "component": "zzgfcslv_state", "where": "context", "entries": ['zzgfcslv']},
    "gfevnt": {"state": "saved", "component": "gfevnt_state", "where": "context", "entries": ['gfevnt']},
    # GFSTEP's step (set by GFSSTP) belongs to the context, like its other GF state.
    "gfstep": {"state": "saved", "component": "gfstep_state", "where": "context",
               "entries": ["gfstep", "gfsstp"], "docs": {"svstep": "the step size (set by GFSSTP)",
                                                         "svinit": "the step size has been set"}},
    # Phase 6: geometry, kernel writers, symbol tables: caches (of names, frames,
    # surfaces, shapes asked for last), first-call setup and call sequences
    # (zzdsksel, zzsinutl, zztanutl), all per context. ZZHOLDD (GF tolerance and
    # reference value) is per context too, so GFSTOL sets the context's tolerance.
    "ana": {"state": "saved", "component": "ana_state", "where": "context", "entries": ['ana']},
    "azlcpo": {"state": "saved", "component": "azlcpo_state", "where": "context", "entries": ['azlcpo']},
    "bltfrm": {"state": "saved", "component": "bltfrm_state", "where": "context", "entries": ['bltfrm']},
    "diags2": {"state": "saved", "component": "diags2_state", "where": "context", "entries": ['diags2']},
    "ev2lin": {"state": "saved", "component": "ev2lin_state", "where": "context", "entries": ['ev2lin']},
    "hyptof": {"state": "saved", "component": "hyptof_state", "where": "context", "entries": ['hyptof']},
    "illum": {"state": "saved", "component": "illum_state", "where": "context", "entries": ['illum']},
    "lspcn": {"state": "saved", "component": "lspcn_state", "where": "context", "entries": ['lspcn']},
    "lxname": {"state": [], "component": None, "entries": ['lxidnt', 'lxdfid', 'lxcsid']},
    "oscelt": {"state": "saved", "component": "oscelt_state", "where": "context", "entries": ['oscelt']},
    "prtenc": {"state": [], "component": None, "entries": ['prtenc', 'prtdec']},
    "pxfrm2": {"state": "saved", "component": "pxfrm2_state", "where": "context", "entries": ['pxfrm2']},
    "repml": {"state": "saved", "component": "repml_state", "where": "context", "entries": ['repml']},
    "scps01": {"state": "saved", "component": "scps01_state", "where": "context", "entries": ['scps01']},
    "sctran": {"state": [], "component": None, "entries": ['scn2id', 'scid2n']},
    "spcac": {"state": "saved", "component": "spcac_state", "where": "context", "entries": ['spcac']},
    "spcrfl": {"state": "saved", "component": "spcrfl_state", "where": "context", "entries": ['spcrfl', 'spcrnl']},
    "spkpv": {"state": "saved", "component": "spkpv_state", "where": "context", "entries": ['spkpv']},
    "surfpt": {"state": "saved", "component": "surfpt_state", "where": "context", "entries": ['surfpt']},
    "zzbquad": {"state": "saved", "component": "zzbquad_state", "where": "context", "entries": ['zzbquad']},
    "zzdiv": {"state": "saved", "component": "zzdiv_state", "where": "context", "entries": ['zzdiv']},
    "zzdskbbl": {"state": "saved", "component": "zzdskbbl_state", "where": "context", "entries": ['zzdskbbl']},
    "zzdsksel": {"state": "saved", "component": "zzdsksel_state", "where": "context", "entries": ['zzdsksbd', 'zzdskbdc', 'zzdsknot', 'zzdsksit', 'zzdskcit', 'zzdskusc', 'zzdskumc', 'zzdskmsc', 'zzdskmmc', 'zzdsksrc', 'zzdskmrc']},
    "zzgetelm": {"state": "saved", "component": "zzgetelm_state", "where": "context", "entries": ['zzgetelm']},
    "zzholdd": {"state": "saved", "component": "zzholdd_state", "where": "context", "entries": ['zzholdd']},
    "zzmult": {"state": "saved", "component": "zzmult_state", "where": "context", "entries": ['zzmult']},
    "zzrtnmat": {"state": "saved", "component": "zzrtnmat_state", "where": "context", "entries": ['zzrtnmat']},
    "zzsfxcor": {"state": "saved", "component": "zzsfxcor_state", "where": "context", "entries": ['zzsfxcor']},
    "zzsglatx": {"state": "saved", "component": "zzsglatx_state", "where": "context", "entries": ['zzsglatx']},
    "zzspin": {"state": "saved", "component": "zzspin_state", "where": "context", "entries": ['zzspin']},
    "zzspkap0": {"state": "saved", "component": "zzspkap0_state", "where": "context", "entries": ['zzspkap0']},
    "zzspkap1": {"state": "saved", "component": "zzspkap1_state", "where": "context", "entries": ['zzspkap1']},
    "dpgrdr": {"state": "saved", "component": "dpgrdr_state", "where": "context", "entries": ['dpgrdr']},
    "drdpgr": {"state": "saved", "component": "drdpgr_state", "where": "context", "entries": ['drdpgr']},
    "edterm": {"state": "saved", "component": "edterm_state", "where": "context", "entries": ['edterm']},
    "getfvn": {"state": "saved", "component": "getfvn_state", "where": "context", "entries": ['getfvn']},
    "pgrrec": {"state": "saved", "component": "pgrrec_state", "where": "context", "entries": ['pgrrec']},
    "recpgr": {"state": "saved", "component": "recpgr_state", "where": "context", "entries": ['recpgr']},
    "subpt": {"state": "saved", "component": "subpt_state", "where": "context", "entries": ['subpt']},
    "subsol": {"state": "saved", "component": "subsol_state", "where": "context", "entries": ['subsol']},
    "tostdo": {"state": "saved", "component": "tostdo_state", "where": "context", "entries": ['tostdo']},
    "zzcnquad": {"state": "saved", "component": "zzcnquad_state", "where": "context", "entries": ['zzcnquad']},
    "zzdsksph": {"state": "saved", "component": "zzdsksph_state", "where": "context", "entries": ['zzdsksph']},
    "zznrddp": {"state": "saved", "component": "zznrddp_state", "where": "context", "entries": ['zzdpinit', 'zzdpsec', 'zzdpper']},
    "zzpdcmpl": {"state": "saved", "component": "zzpdcmpl_state", "where": "context", "entries": ['zzpdcmpl']},
    "zzsrfker": {"state": "saved", "component": "zzsrfker_state", "where": "context", "entries": ['zzsrfker']},
    "dpspce": {"state": "saved", "component": "dpspce_state", "where": "context", "entries": ['dpspce']},
    "incnsg": {"state": "saved", "component": "incnsg_state", "where": "context", "entries": ['incnsg']},
    "xfmsta": {"state": "saved", "component": "xfmsta_state", "where": "context", "entries": ['xfmsta']},
    "zzsrftrn": {"state": "saved", "component": "zzsrftrn_state", "where": "context", "entries": ['zzsrfn2c', 'zzsrfc2n', 'zzsrftrk']},
    "zzelvupy": {"state": "saved", "component": "zzelvupy_state", "where": "context", "entries": ['zzelvupy']},
    "zzinpdt": {"state": "saved", "component": "zzinpdt_state", "where": "context", "entries": ['zzinpdt']},
    "zzrytlat": {"state": "saved", "component": "zzrytlat_state", "where": "context", "entries": ['zzrytlat']},
    "zzrytpdt": {"state": "saved", "component": "zzrytpdt_state", "where": "context", "entries": ['zzrytpdt']},
    "dskx02": {"state": "saved", "component": "dskx02_state", "where": "context", "entries": ['dskx02']},
    "srfxpt": {"state": "saved", "component": "srfxpt_state", "where": "context", "entries": ['srfxpt']},
    "tangpt": {"state": "saved", "component": "tangpt_state", "where": "context", "entries": ['tangpt']},
    "zzptpl02": {"state": "saved", "component": "zzptpl02_state", "where": "context", "entries": ['zzptpl02']},
    "zzdsksbf": {"state": "saved", "component": "zzdsksbf_state", "where": "context", "entries": ['zzsbfxr', 'zzsbfxri', 'zzsbfnrm']},
    "dskxsi": {"state": "saved", "component": "dskxsi_state", "where": "context", "entries": ['dskxsi']},
    "dskxv": {"state": "saved", "component": "dskxv_state", "where": "context", "entries": ['dskxv']},
    "illumg": {"state": "saved", "component": "illumg_state", "where": "context", "entries": ['illumg']},
    "srfnrm": {"state": "saved", "component": "srfnrm_state", "where": "context", "entries": ['srfnrm']},
    "zzsinutl": {"state": "saved", "component": "zzsinutl_state", "where": "context", "entries": ['zzsuelin', 'zzsudski', 'zzraysfx', 'zzmaxrad', 'zzminrad', 'zzraynp']},
    "illumf": {"state": "saved", "component": "illumf_state", "where": "context", "entries": ['illumf']},
    "latsrf": {"state": "saved", "component": "latsrf_state", "where": "context", "entries": ['latsrf']},
    "sincpt": {"state": "saved", "component": "sincpt_state", "where": "context", "entries": ['sincpt']},
    "subpnt": {"state": "saved", "component": "subpnt_state", "where": "context", "entries": ['subpnt']},
    "subslr": {"state": "saved", "component": "subslr_state", "where": "context", "entries": ['subslr']},
    "zztanutl": {"state": "saved", "component": "zztanutl_state", "where": "context", "entries": ['zztanini', 'zztansta']},
    "limbpt": {"state": "saved", "component": "limbpt_state", "where": "context", "entries": ['limbpt']},
    "termpt": {"state": "saved", "component": "termpt_state", "where": "context", "entries": ['termpt']},
    # The SPK, CK, PCK, SCLK and time API (phase 5): caches of the names and
    # corrections asked for last, first-call setup, segments being written
    # (SGSEQW) and call sequences (ZZCVSTAT): per context.
    "deltet": {"state": "saved", "component": "deltet_state", "where": "context", "entries": ['deltet']},
    "et2utc": {"state": "saved", "component": "et2utc_state", "where": "context", "entries": ['et2utc']},
    "scanit": {"state": [], "component": None, "entries": ['scanpr', 'scan']},
    "sgseqw": {"state": "saved", "component": "sgseqw_state", "where": "context", "entries": ['sgbwfs', 'sgbwvs', 'sgwfpk', 'sgwvpk', 'sgwes']},
    "spkgeo": {"state": "saved", "component": "spkgeo_state", "where": "context", "entries": ['spkgeo']},
    "spkgps": {"state": "saved", "component": "spkgps_state", "where": "context", "entries": ['spkgps']},
    "spks01": {"state": "saved", "component": "spks01_state", "where": "context", "entries": ['spks01']},
    "spks02": {"state": "saved", "component": "spks02_state", "where": "context", "entries": ['spks02']},
    "spks03": {"state": "saved", "component": "spks03_state", "where": "context", "entries": ['spks03']},
    "spks19": {"state": "saved", "component": "spks19_state", "where": "context", "entries": ['spks19']},
    "spks20": {"state": "saved", "component": "spks20_state", "where": "context", "entries": ['spks20']},
    "spks21": {"state": "saved", "component": "spks21_state", "where": "context", "entries": ['spks21']},
    "zzcvstat": {"state": "saved", "component": "zzcvstat_state", "where": "context", "entries": ['zzcvxsta', 'zzcvssta']},
    "bodeul": {"state": "saved", "component": "bodeul_state", "where": "context", "entries": ['bodeul']},
    "ckgp": {"state": "saved", "component": "ckgp_state", "where": "context", "entries": ['ckgp']},
    "ckgpav": {"state": "saved", "component": "ckgpav_state", "where": "context", "entries": ['ckgpav']},
    "scdecd": {"state": "saved", "component": "scdecd_state", "where": "context", "entries": ['scdecd']},
    "scencd": {"state": "saved", "component": "scencd_state", "where": "context", "entries": ['scencd']},
    "spkapo": {"state": "saved", "component": "spkapo_state", "where": "context", "entries": ['spkapo']},
    "spkltc": {"state": "saved", "component": "spkltc_state", "where": "context", "entries": ['spkltc']},
    "timout": {"state": "saved", "component": "timout_state", "where": "context", "entries": ['timout']},
    "spkapp": {"state": "saved", "component": "spkapp_state", "where": "context", "entries": ['spkapp']},
    "spkaps": {"state": "saved", "component": "spkaps_state", "where": "context", "entries": ['spkaps']},
    "spkezp": {"state": "saved", "component": "spkezp_state", "where": "context", "entries": ['spkezp']},
    "zzspkflt": {"state": "saved", "component": "zzspkflt_state", "where": "context", "entries": ['zzspkflt']},
    "spkacs": {"state": "saved", "component": "spkacs_state", "where": "context", "entries": ['spkacs']},
    "spkpos": {"state": "saved", "component": "spkpos_state", "where": "context", "entries": ['spkpos']},
    "zzspkfao": {"state": "saved", "component": "zzspkfao_state", "where": "context", "entries": ['zzspkfao']},
    "zzspkfap": {"state": "saved", "component": "zzspkfap_state", "where": "context", "entries": ['zzspkfap']},
    "spkez": {"state": "saved", "component": "spkez_state", "where": "context", "entries": ['spkez']},
    "zzspkfat": {"state": "saved", "component": "zzspkfat_state", "where": "context", "entries": ['zzspkfat']},
    "zzspkfzo": {"state": "saved", "component": "zzspkfzo_state", "where": "context", "entries": ['zzspkfzo']},
    "spkcvo": {"state": "saved", "component": "spkcvo_state", "where": "context", "entries": ['spkcvo']},
    "spkezr": {"state": "saved", "component": "spkezr_state", "where": "context", "entries": ['spkezr']},
    "zzspkfzt": {"state": "saved", "component": "zzspkfzt_state", "where": "context", "entries": ['zzspkfzt']},
    "spkcvt": {"state": "saved", "component": "spkcvt_state", "where": "context", "entries": ['spkcvt']},
    # The rest of the frame system (phase 5): caches (of the frame data, the
    # last frames and corrections asked for) and first-call setup, per context.
    "frmget": {"state": "saved", "component": "frmget_state", "where": "context", "entries": ['frmget']},
    "pxform": {"state": "saved", "component": "pxform_state", "where": "context", "entries": ['pxform']},
    "rotget": {"state": "saved", "component": "rotget_state", "where": "context", "entries": ['rotget']},
    "spke10": {"state": [], "component": None, "entries": ['spke10']},
    "sxform": {"state": "saved", "component": "sxform_state", "where": "context", "entries": ['sxform']},
    "tisbod": {"state": "saved", "component": "tisbod_state", "where": "context", "entries": ['tisbod']},
    "tkfram": {"state": "saved", "component": "tkfram_state", "where": "context", "entries": ['tkfram']},
    "zzdynfr0": {"state": "saved", "component": "zzdynfr0_state", "where": "context", "entries": ['zzdynfr0']},
    "zzdynfrm": {"state": "saved", "component": "zzdynfrm_state", "where": "context", "entries": ['zzdynfrm']},
    "zzdynrot": {"state": "saved", "component": "zzdynrot_state", "where": "context", "entries": ['zzdynrot']},
    "zzdynrt0": {"state": "saved", "component": "zzdynrt0_state", "where": "context", "entries": ['zzdynrt0']},
    "zzfrmgt0": {"state": "saved", "component": "zzfrmgt0_state", "where": "context", "entries": ['zzfrmgt0']},
    "zzfrmgt1": {"state": "saved", "component": "zzfrmgt1_state", "where": "context", "entries": ['zzfrmgt1']},
    "zzrotgt0": {"state": "saved", "component": "zzrotgt0_state", "where": "context", "entries": ['zzrotgt0']},
    "zzrotgt1": {"state": "saved", "component": "zzrotgt1_state", "where": "context", "entries": ['zzrotgt1']},
    "zzspkac0": {"state": "saved", "component": "zzspkac0_state", "where": "context", "entries": ['zzspkac0']},
    "zzspkac1": {"state": "saved", "component": "zzspkac1_state", "where": "context", "entries": ['zzspkac1']},
    "zzspkas0": {"state": "saved", "component": "zzspkas0_state", "where": "context", "entries": ['zzspkas0']},
    "zzspkas1": {"state": "saved", "component": "zzspkas1_state", "where": "context", "entries": ['zzspkas1']},
    "zzspkez0": {"state": "saved", "component": "zzspkez0_state", "where": "context", "entries": ['zzspkez0']},
    "zzspkez1": {"state": "saved", "component": "zzspkez1_state", "where": "context", "entries": ['zzspkez1']},
    "zzspkgo0": {"state": "saved", "component": "zzspkgo0_state", "where": "context", "entries": ['zzspkgo0']},
    "zzspkgo1": {"state": "saved", "component": "zzspkgo1_state", "where": "context", "entries": ['zzspkgo1']},
    "zzspkgp0": {"state": "saved", "component": "zzspkgp0_state", "where": "context", "entries": ['zzspkgp0']},
    "zzspkgp1": {"state": "saved", "component": "zzspkgp1_state", "where": "context", "entries": ['zzspkgp1']},
    "zzspklt0": {"state": "saved", "component": "zzspklt0_state", "where": "context", "entries": ['zzspklt0']},
    "zzspklt1": {"state": "saved", "component": "zzspklt1_state", "where": "context", "entries": ['zzspklt1']},
    "zzspkpa0": {"state": "saved", "component": "zzspkpa0_state", "where": "context", "entries": ['zzspkpa0']},
    "zzspkpa1": {"state": "saved", "component": "zzspkpa1_state", "where": "context", "entries": ['zzspkpa1']},
    "zzspkzp0": {"state": "saved", "component": "zzspkzp0_state", "where": "context", "entries": ['zzspkzp0']},
    "zzspkzp1": {"state": "saved", "component": "zzspkzp1_state", "where": "context", "entries": ['zzspkzp1']},
    "zzswfxfm": {"state": "saved", "component": "zzswfxfm_state", "where": "context", "entries": ['zzswfxfm']},
    # Time, SCLK, rotations, the SPK/PCK/CK records and the frame names
    # (phase 5): caches, first-call tables and calling sequences (SGP4,
    # ZZVSTRNG) are per context, as SPICELIB's saved variables are per
    # program; the TIMDEF defaults are settings of the instance.
    # CHGIRF: the default inertial frame (IRFDEF) is a setting of the
    # instance; the rotations computed on the first call a per-context cache.
    "chgirf": {"state": ["dframe"], "component": "chgirf_state",
               "context_state": ["ready", "trans"], "context_component": "chgirf_cache",
               "docs": {"dframe": "the default inertial frame (set by IRFDEF; 0: none)",
                        "ready": "the rotations have been computed",
                        "trans": "rotation (3x3, by columns) from J2000 to each frame"},
               "entries": ['irfrot', 'irfnum', 'irfnam', 'irfdef']},
    "ckmeta": {"state": "saved", "component": "ckmeta_state", "where": "context", "entries": ['ckmeta']},
    "ckr03": {"state": "saved", "component": "ckr03_state", "where": "context", "entries": ['ckr03']},
    "ckr05": {"state": "saved", "component": "ckr05_state", "where": "context", "entries": ['ckr05']},
    "ckr06": {"state": "saved", "component": "ckr06_state", "where": "context", "entries": ['ckr06']},
    "etcal": {"state": "saved", "component": "etcal_state", "where": "context", "entries": ['etcal']},
    "framex": {"state": "saved", "component": "framex_state", "where": "context", "entries": ['namfrm', 'frmnam', 'frinfo', 'cidfrm', 'cnmfrm', 'ccifrm']},
    "jul2gr": {"state": "first rdayg rdayj offstj offstg".split(), "component": "jul2gr_state", "where": "context", "entries": ['jul2gr', 'gr2jul']},
    "prop2b": {"state": "saved", "component": "prop2b_state", "where": "context", "entries": ['prop2b']},
    "sc01": {"state": "saved", "component": "sc01_state", "where": "context", "entries": ['sctk01', 'scfm01', 'scte01', 'scet01', 'scec01', 'scty01', 'scpr01']},
    "sclu01": {"state": "saved", "component": "sclu01_state", "where": "context", "entries": ['scli01', 'scld01']},
    "sgmeta": {"state": "saved", "component": "sgmeta_state", "where": "context", "entries": ['sgmeta']},
    "spke01": {"state": [], "component": None, "entries": ['spke01']},
    "spke08": {"state": [], "component": None, "entries": ['spke08']},
    "spke21": {"state": [], "component": None, "entries": ['spke21']},
    "spkr10": {"state": [], "component": None, "entries": ['spkr10']},
    "spkr19": {"state": "saved", "component": "spkr19_state", "where": "context", "entries": ['spkr19']},
    "stmp03": {"state": "saved", "component": "stmp03_state", "where": "context", "entries": ['stmp03']},
    "str2et": {"state": "saved", "component": "str2et_state", "where": "context", "entries": ['str2et']},
    "timdef": {"state": "saved", "component": "timdef_state", "entries": ['timdef']},
    "ttrans": {"state": "saved", "component": "ttrans_state", "where": "context", "entries": ['ttrans']},
    "unitim": {"state": "saved", "component": "unitim_state", "where": "context", "entries": ['unitim']},
    "xf2eul": {"state": [], "component": "xf2eul_state", "where": "context", "entries": ['xf2eul', 'eul2xf']},
    "zzdnpt": {"state": "saved", "component": "zzdnpt_state", "where": "context", "entries": ['zzdnpt']},
    "zzmobliq": {"state": "saved", "component": "zzmobliq_state", "where": "context", "entries": ['zzmobliq']},
    "zzscad01": {"state": "saved", "component": "zzscad01_state", "where": "context", "entries": ['zzscad01']},
    "zzsclk": {"state": "saved", "component": "zzsclk_state", "where": "context", "entries": ['zzsclk']},
    "zzsgp4": {"state": "saved", "component": "zzsgp4_state", "where": "context", "entries": ['xxsgp4i', 'xxsgp4e']},
    "zzvstrng": {"state": "saved", "component": "zzvstrng_state", "where": "context", "entries": ['zzvstrng', 'zzvststr', 'zzvsbstr']},
    "zzwahr": {"state": "saved", "component": "zzwahr_state", "where": "context", "entries": ['zzwahr']},
    # Segment buffers: the file tables belong to the instance; the body
    # (instrument) tables, segment lists and searches in progress are a cache
    # per context, discarded when any context loads or unloads a file (see
    # segment_epoch in pepper_spice).
    **{n: {"state": "fthan ftnum next nft".split(), "component": n,
           "context_state": """btbeg btbod btchkp btexp bthfs btlb btlfs btprvd btprvh btprvi
                               btruex btub nbt stdes sthan stidnt stpool""".split(),
           "context_component": n + "_cache", "context_epoch": True,
           "context_init": ["call ctx%lnkini ( stsize, {c}%stpool )"],
           "epoch_entries": [lo, un], "entries": [lo, un, sfs]}
       for n, lo, un, sfs in (("spkbsr", "spklef", "spkuef", "spksfs"),
                              ("pckbsr", "pcklof", "pckuof", "pcksfs"))},
    # The kernel manager: the table of loaded kernels belongs to the
    # instance, like the kernel pool and the files themselves; so does the
    # FURNSH watcher on the meta-kernel variables (set on the first call).
    "keeper": {"state": "files handls srces types loaded known first".split(),
               "component": "keeper",
               "docs": {"files": "name of each kernel loaded through FURNSH",
                        "handls": "handle of each loaded kernel (0 for a text kernel)",
                        "srces": "index in files of the meta-kernel each kernel was loaded "
                                 "from (0: loaded directly)",
                        "types": "type (SPK, CK, PCK, DSK, EK, TEXT, META) of each loaded kernel",
                        "loaded": "number of kernels loaded",
                        "known": "the meta-kernel variables (KERNELS_TO_LOAD, PATH_SYMBOLS, "
                                 "PATH_VALUES) watched by FURNSH",
                        "first": "the watcher has not been set yet"},
               "entries": ["furnsh", "ktotal", "kdata", "kinfo", "kclear", "unload"]},
    "ckbsr": {"state": "fthan ftnum next nft".split(), "component": "ckbsr",
              "context_state": """alpha avneed findex fnd fresub iindex itbeg itchkp itexp ithfs
                                  itins itlb itlfs itprvd itprvf itprvh itprvi itub itruex newsch
                                  nit omega reqt savep savtol scinst slbeg status stdcd sthan
                                  sticd stidnt stpool top""".split(),
              "context_component": "ckbsr_cache", "context_epoch": True,
              "context_init": ["call ctx%lnkini ( stsize, {c}%stpool )"],
              "epoch_entries": ["cklpf", "ckupf"],
              "entries": ["cklpf", "ckupf", "ckbss", "cksns", "ckhave"]},
    "zzdskbsr": {"state": "fthan ftnum next nft dskctr pass1".split(), "component": "zzdskbsr",
                 "context_state": """begsch findex fnd bindex btbeg btexp bthfs btbod btlfs nbt
                                     savep svbody status stdlad stdskd sthan stpool top""".split(),
                 "context_component": "zzdskbsr_cache", "context_epoch": True,
                 "context_init": ["call ctx%lnkini ( stsize, {c}%stpool )"],
                 "epoch_entries": ["zzdsklsf", "zzdskusf"],
                 "entries": ["zzdsklsf", "zzdskusf", "zzdskbss", "zzdsksns", "zzdskchk"]},
    # Saved caches of the last name/code translated: per context.
    "bodvrd": {"state": "saved", "component": "bodvrd_cache", "where": "context", "entries": ['bodvrd']},
    # The handle manager: the file table belongs to the instance; the units
    # (and the tables of format names built on the first call) to the context,
    # so that each thread opens and recycles its own units.
    "zzddhman": {"state": "nft ftabs ftamh ftarc ftbff fthan ftnam ftrtm ftmnm next".split(),
                 "component": "zzddhman",
                 "context_state": """first opnfst stramh strarc strbff natbff supbff
                                     numsup nut utcst uthan utlck utlun reqcnt""".split(),
                 "context_component": "zzddhman_units",
                 "docs": {"nft": "number of files in the file table",
                          "ftabs": "absolute value of each file's handle",
                          "ftamh": "access method of each file",
                          "ftarc": "architecture (DAF, DAS) of each file",
                          "ftbff": "binary file format of each file",
                          "fthan": "handle of each file",
                          "ftnam": "name of each file",
                          "ftrtm": "length of each file name",
                          "ftmnm": "unique number (from the file's attributes) of each file",
                          "next": "the next handle number (handles are never reused)",
                          "first": "the tables of names have not been built yet",
                          "opnfst": "no file has been opened yet (platform check)",
                          "stramh": "names of the access methods",
                          "strarc": "names of the architectures",
                          "strbff": "names of the binary file formats",
                          "natbff": "the native binary file format",
                          "supbff": "the supported binary file formats",
                          "numsup": "number of supported binary file formats",
                          "nut": "number of entries in the unit table",
                          "utcst": "cost (time of the last request) of each unit",
                          "uthan": "handle of the file connected to each unit",
                          "utlck": "the unit is locked to its file",
                          "utlun": "the units",
                          "reqcnt": "request counter (for the costs)"},
                 "entries": ['zzddhopn', 'zzddhcls', 'zzddhhlu', 'zzddhunl', 'zzddhnfo', 'zzddhisn', 'zzddhfnh', 'zzddhluh']},
    # First-call tables of format and platform information: per context.
    **{n: {"state": "saved", "component": n, "where": "context", "entries": [n]} for n in
       "zzplatfm zzftpstr zzftpchk zzgetbff zzddhgsd zzddhmnm zzddhppf zzxlatei zzddhnfc".split()},
    # DAF: the file table belongs to the instance; searches and arrays being
    # written, record buffers and first-call tables to the context.
    "dafah": {"state": "nft fthan ftlnk ftnd ftni fhlist first".split(), "component": "dafah",
              "docs": {"nft": "number of DAFs in the file table",
                       "fthan": "handle of each DAF",
                       "ftlnk": "number of links (opens) of each DAF",
                       "ftnd": "number of double precision components of the summaries",
                       "ftni": "number of integer components of the summaries",
                       "fhlist": "set of the handles of the open DAFs",
                       "first": "the handle set has not been initialized yet"},
              "entries": ['dafopr', 'dafopw', 'dafonw', 'dafopn', 'dafcls', 'dafhsf', 'dafhlu', 'dafluh', 'dafhfn', 'daffnh', 'dafhof', 'dafsih']},
    "daffa": {"state": ['stfh', 'stprev', 'stthis', 'stnext', 'stnseg', 'stcurr', 'stnr', 'sthvnr', 'stsr', 'stpool', 'sthead', 'stfptr', 'first'], "component": "daffa", "where": "context", "entries": ['dafbfs', 'daffna', 'dafbbs', 'daffpa', 'dafgs', 'dafgn', 'dafgh', 'dafrs', 'dafrn', 'dafws', 'dafcs']},
    "dafana": {"state": ['stfh', 'stifnm', 'staddg', 'stfrst', 'stlast', 'stfree', 'stbegn', 'stlsum', 'stname', 'stpool', 'sthead', 'stfptr', 'first'], "component": "dafana", "where": "context", "entries": ['dafbna', 'dafada', 'dafena', 'dafcad']},
    "dafrwd": {"state": "saved", "component": "dafrwd", "where": "context", "entries": ['dafgdr', 'dafgsr', 'dafrdr', 'dafwdr', 'dafnrr']},
    "dafps": {"state": [], "component": None, "entries": ['dafps', 'dafus']},
    "dafrwa": {"state": [], "component": None, "entries": ['dafrwa', 'dafarw']},
    # (component <name>_state: a component may not have the name of a
    # type-bound procedure)
    **{n: {"state": "saved", "component": n + "_state", "where": "context", "entries": [n]} for n in
       "dafec dafwda zzdafgdr zzdafgsr zzdafgfr zzdafnfr zzxlated hx2dp".split()},
    # DAS: the file table belongs to the instance; record buffers, the
    # address cache, comment reading and first-call tables to the context.
    "dasfm": {"state": "fthan ftacc ftlnk ftsum pool fthead fhlist pass1".split(),
              "component": "dasfm",
              "docs": {"fthan": "handle of each DAS", "ftacc": "access method of each DAS",
                       "ftlnk": "number of links (opens) of each DAS",
                       "ftsum": "file summary of each DAS (free records and addresses, last "
                                "records and words of each data type)",
                       "pool": "linked-list pool of the file table",
                       "fthead": "head of the list of the file table's entries",
                       "fhlist": "set of the handles of the open DASs",
                       "pass1": "the file table has not been initialized yet"},
              "entries": ['dasopr', 'dasopw', 'dasonw', 'dasopn', 'dasops', 'dasllc', 'dashfs', 'dasufs', 'dashlu', 'dasluh', 'dashfn', 'dasfnh', 'dashof', 'dassih', 'dasham']},
    "dasrwr": {"state": ['rcbufc', 'rcbufd', 'rcbufi', 'rnbufc', 'rnbufd', 'rnbufi', 'hnbufc', 'hnbufd', 'hnbufi', 'upbufc', 'upbufd', 'upbufi', 'poolc', 'poold', 'pooli', 'headc', 'headd', 'headi', 'usedc', 'usedd', 'usedi', 'pass1'], "component": "dasrwr", "where": "context", "entries": ['dasrrd', 'dasrri', 'dasrrc', 'daswrd', 'daswri', 'daswrc', 'dasurd', 'dasuri', 'dasurc', 'daswbr']},
    "dasa2l": {"state": ['fidx', 'next', 'nfiles', 'nw', 'prev', 'prvhan', 'rngloc', 'tbbase', 'tbfwrd', 'tbhan', 'tbmxad', 'tbsize', 'fast', 'prvok', 'tbfast', 'tbrdon'], "component": "dasa2l_state", "where": "context", "entries": ['dasa2l']},
    **{n: {"state": "saved", "component": n + "_state", "where": "context", "entries": [n]} for n in
       "dasadc dasadd dasadi dascls dasec zzdasgrd zzdasgri zzdasnfr zzdasrfr wrencc".split()},
    # DSK tolerances and margins (set by DSKSTL): settings of the instance.
    "dsktol": {"state": ["dppars"], "component": "dsktol",
               "docs": {"dppars": "the values of the DSK tolerance and margin parameters"}, "entries": ['dskgtl', 'dskstl']},
    # Caches of the last segment's data base address: per context.
    "dskd02": {"state": "saved", "component": "dskd02_state", "where": "context",
               "entries": ["dskd02"]},
    "dski02": {"state": "saved", "component": "dski02_state", "where": "context",
               "entries": ["dski02"]},
    "zzhsc": {"state": [], "component": None, "entries": ['zzhscini', 'zzhscadd', 'zzhscchk', 'zzhscavl', 'zzhscinf']},
    "zzhsi": {"state": [], "component": None, "entries": ['zzhsiini', 'zzhsiadd', 'zzhsichk', 'zzhsiavl', 'zzhsiinf']},
    # Built-in body names and codes (tables built on the first call).
    "zzbodblt": {"state": ['bltnam', 'bltnor', 'bltcod', 'first'], "component": "zzbodblt", "where": "context", "entries": ['zzbodget', 'zzbodlst']},
    # Body name/code tables derived from the pool: a cache, per context.
    "zzbodtrn": {"state": ['defnam', 'defnor', 'defcod', 'defsiz', 'dnmlst', 'dnmpol', 'dnmnms', 'dnmidx', 'didlst', 'didpol', 'didids', 'dididx', 'kernam', 'kercod', 'knmlst', 'knmpol', 'knmnms', 'knmidx', 'kidlst', 'kidpol', 'kidids', 'kididx', 'subctr', 'pulctr', 'bodchg', 'extker', 'nodata', 'i', 'first'], "component": "zzbodtrn", "where": "context", "entries": ['zzbodn2c', 'zzbodc2n', 'zzboddef', 'zzbodkik', 'zzbodrst', 'zzbctrck']},
    "zzctr": {"state": [], "component": None, "entries": ['zzctruin', 'zzctrsin', 'zzctrinc', 'zzctrchk']},
    "tcheck": {"state": ["dochck"], "component": "tcheck", "entries": ['tcheck', 'tparch', 'tchckd'], "docs": {'dochck': 'check the components of parsed times (set by TPARCH)'}},
    "texpyr": {"state": ["centry", "lbound"], "component": "texpyr", "entries": ['texpyr', 'tsetyr'], "docs": {'centry': 'the century assumed for two-digit years', 'lbound': 'the lower bound of the 100-year window for two-digit years'}},
    # Saves the tokens of the string being parsed between calls: per context.
    "zztime": {"state": ['rep', 'size', 'begs', 'ends', 'pbegs', 'pends', 'pictur', 'names', 'picerr', 'tknerr', 'hms', 'blank', 'to', 'pto', 'case', 'mnsize', 'wksize', 'width', 'recog', 'mnmrk', 'wkday', 'class', 'f', 'l', 'ampm', 'first'], "component": "zztime", "where": "context", "entries": ['zzcmbt', 'zzgrep', 'zzispt', 'zzist', 'zznote', 'zzremt', 'zzsubt', 'zztokns', 'zzunpck', 'zzvalt']},
    "rdtext": {"state": "units n lstfil lstunt index".split(), "component": "rdtext", "entries": ['rdtext', 'cltext'], "docs": {'units': 'units of the files open for reading', 'n': 'number of files open', 'lstfil': 'the file read last', 'lstunt': 'the unit of the file read last', 'index': 'index in units of the file read last'}},
    "rdker": {"state": "file linnum status frstim begdat begtxt".split(), "component": "rdker", "entries": ['rdknew', 'rdkdat', 'rdklin'], "docs": {'file': 'the text kernel being read', 'linnum': 'number of the last line read', 'status': 'reading text, data, or at the end of the file', 'frstim': 'no kernel has been opened yet', 'begdat': 'the begin-data marker', 'begtxt': 'the begin-text marker'}},
    "pool": {"state": """pnames namlst nmpool datlst chpool dppool chvals dpvals
                         wtvars wtpool wtagnt wtptrs active agents notify first subctr
                         begdat begtxt""".split(), "component": "pool", "entries": ['clpool', 'ldpool', 'rtpool', 'expool', 'wrpool', 'swpool', 'cvpool', 'gcpool', 'gdpool', 'gipool', 'dtpool', 'pcpool', 'pdpool', 'pipool', 'lmpool', 'szpool', 'dvpool', 'gnpool', 'dwpool', 'zzvupool', 'zzpctrck'], "docs": {'pnames': 'names of the kernel pool variables', 'namlst': 'heads of the hash collision lists (by hash value of the name)', 'nmpool': 'linked-list pool of the name collision lists', 'datlst': 'heads of the value lists of the variables (positive: numeric, negative: character)', 'chpool': 'linked-list pool of the character values', 'dppool': 'linked-list pool of the numeric values', 'chvals': 'character values', 'dpvals': 'numeric values', 'wtvars': 'watcher table: the watched variables (a set)', 'wtpool': 'linked-list pool of the watcher table', 'wtagnt': 'watcher table: the agents watching each variable', 'wtptrs': 'watcher table: pointers into wtpool for each watched variable', 'active': 'set of the agents that have watchers', 'agents': 'work set of agents', 'notify': 'set of the agents to notify of an update', 'first': 'the pool has not been initialized yet', 'subctr': 'pool state counter (changes when the pool changes)', 'begdat': 'the begin-data marker of text kernels', 'begtxt': 'the begin-text marker of text kernels'}},
}

# Legacy routines (still in src/spicelib) that moved code may call for now,
# with the reason. Remove an entry when the routine moves.
ALLOWED_LEGACY = {}

# Callbacks: the procedures that SPICELIB passes as arguments (EXTERNAL
# arguments). Each has an abstract interface in pepper_instance with the
# context as first argument (generated by tools/callbacks.py) and, for the
# legacy interface, a bridge from SPICELIB's form without the context
# (src/legacy/pepper_legacy_callbacks.f90). kind: "subroutine" or
# "logical function"; args: (name, type, intent, dimensions, description).
CALLBACKS = {
    "zzdsk_compare": {
        "kind": "logical function",
        "doc": "A segment selection function of the DSK segment buffer (ZZDSKSNS): true if the "
               "segment satisfies the search's criteria. SPICELIB's are ZZDSKBDC and ZZDSKNOT.",
        "args": [("handle", "integer(ip)", "in", "", "handle of the DSK file of the segment"),
                 ("dladsc", "integer(ip)", "in", "( * )", "DLA descriptor of the segment"),
                 ("dskdsc", "real(wp)", "in", "( * )", "DSK descriptor of the segment")]},
    "zzspk_state": {
        "kind": "subroutine",
        "doc": "A state routine for the SPK routines that take one (ZZSPKFAO, ZZSPKFAP, "
               "ZZSPKFAT, ZZSPKFLT, ZZSPKFZO, ZZSPKFZT): the state of an object relative to the "
               "solar system barycenter at an epoch, in an inertial frame. SPICELIB's is ZZCVXSTA.",
        "args": [("et", "real(wp)", "in", "", "epoch (TDB seconds past J2000)"),
                 ("ref", "character(*)", "in", "", "inertial reference frame of the state"),
                 ("center", "integer(ip)", "out", "", "ID code of the center of motion"),
                 ("state", "real(wp)", "out", "( 6 )", "state of the object")]},
    "zzsfx_near": {
        "kind": "subroutine",
        "doc": "The near point on the target's surface to a ray (ZZSFXCOR); SPICELIB's is ZZRAYNP.",
        "args": [("vertex", "real(wp)", "in", "( 3 )", "the ray's vertex"),
                 ("raydir", "real(wp)", "in", "( 3 )", "the ray's direction vector"),
                 ("et", "real(wp)", "in", "", "evaluation epoch (TDB seconds past J2000)"),
                 ("pnear", "real(wp)", "out", "( 3 )", "near point to the ray on the surface"),
                 ("dist", "real(wp)", "out", "", "distance between the near point and the ray")]},
    "zzsfx_maxrad": {
        "kind": "subroutine",
        "doc": "The radius of the target's outer bounding sphere (ZZSFXCOR); SPICELIB's is ZZMAXRAD.",
        "args": [("maxrad", "real(wp)", "out", "", "radius of the outer bounding sphere")]},
    "zzsfx_rayx": {
        "kind": "subroutine",
        "doc": "The intercept of a ray with the target's surface (ZZSFXCOR); SPICELIB's is ZZRAYSFX.",
        "args": [("vertex", "real(wp)", "in", "( 3 )", "the ray's vertex"),
                 ("raydir", "real(wp)", "in", "( 3 )", "the ray's direction vector"),
                 ("et", "real(wp)", "in", "", "evaluation epoch (TDB seconds past J2000)"),
                 ("spoint", "real(wp)", "out", "( 3 )", "surface intercept point"),
                 ("found", "logical", "out", "", "the intercept exists")]},
    "zztan_cond": {
        "kind": "subroutine",
        "doc": "The occultation state of a ray at an angle (ZZTANSLV); SPICELIB's is ZZTANSTA.",
        "args": [("angle", "real(wp)", "in", "", "angle of the ray"),
                 ("ocultd", "logical", "out", "", "the ray is occulted"),
                 ("point", "real(wp)", "out", "( 3 )", "tangent point on the target")]},
    "gf_step": {
        "kind": "subroutine",
        "doc": "A GF step size routine: the time step to take from a time. SPICELIB's is GFSTEP.",
        "args": [("time", "real(wp)", "in", "", "the time (TDB seconds past J2000)"),
                 ("step", "real(wp)", "out", "", "the time step (seconds)")]},
    "gf_refine": {
        "kind": "subroutine",
        "doc": "A GF refinement routine: a time between two times at which a condition differs. "
               "SPICELIB's is GFREFN.",
        "args": [("t1", "real(wp)", "in", "", "one of the times"),
                 ("t2", "real(wp)", "in", "", "the other time"),
                 ("s1", "logical", "in", "", "the condition at T1"),
                 ("s2", "logical", "in", "", "the condition at T2"),
                 ("t", "real(wp)", "out", "", "the time between T1 and T2")]},
    "gf_rep_init": {
        "kind": "subroutine",
        "doc": "A GF progress report initialization routine. SPICELIB's is GFREPI.",
        "args": [("window", "real(wp)", "in", "( lbcell : * )", "the window over which the search runs"),
                 ("begmss", "character(*)", "in", "", "beginning of the text of the report"),
                 ("endmss", "character(*)", "in", "", "end of the text of the report")]},
    "gf_rep_update": {
        "kind": "subroutine",
        "doc": "A GF progress report update routine. SPICELIB's is GFREPU.",
        "args": [("ivbeg", "real(wp)", "in", "", "start time of the work interval"),
                 ("ivend", "real(wp)", "in", "", "end time of the work interval"),
                 ("time", "real(wp)", "in", "", "current time of the search")]},
    "gf_rep_final": {
        "kind": "subroutine",
        "doc": "A GF progress report finalization routine. SPICELIB's is GFREPF.",
        "args": []},
    "gf_bail": {
        "kind": "logical function",
        "doc": "A GF interrupt check: true if the search is to stop. SPICELIB's is GFBAIL.",
        "args": []},
    "gf_cond": {
        "kind": "subroutine",
        "doc": "A GF condition of time (ZZGFREL, ZZGFSOLV): whether a condition holds, or "
               "whether a quantity is decreasing, at a time.",
        "args": [("et", "real(wp)", "in", "", "the time (TDB seconds past J2000)"),
                 ("xbool", "logical", "out", "", "the condition at ET")]},
    "gf_ref": {
        "kind": "subroutine",
        "doc": "A GF routine that takes the reference value of a search (ZZGFREL); SPICELIB's "
               "is ZZGFREF.",
        "args": [("refval", "real(wp)", "in", "", "the reference value")]},
    "gf_boolean": {
        "kind": "subroutine",
        "doc": "A GF condition of a scalar function of time (GFUDB, GFUDS, ZZGFRELX, "
               "ZZGFSOLVX): whether a condition on the function holds at a time, or whether "
               "the function is decreasing (UDQDEC).",
        "args": [("udfunc", "procedure(gf_scalar)", "", "", "the scalar function"),
                 ("et", "real(wp)", "in", "", "the time (TDB seconds past J2000)"),
                 ("xbool", "logical", "out", "", "the condition at ET")]},
    # (the "is decreasing" callbacks have the same arguments as the conditions,
    # but a type of their own: the legacy bridges keep one routine per type,
    # and ZZGFREL, ZZGFRELX take both)
    "gf_decr": {
        "kind": "subroutine",
        "doc": "A GF routine that tells whether a quantity is decreasing at a time (ZZGFREL).",
        "args": [("et", "real(wp)", "in", "", "the time (TDB seconds past J2000)"),
                 ("xbool", "logical", "out", "", "the quantity is decreasing at ET")]},
    "gf_decr_x": {
        "kind": "subroutine",
        "doc": "A GF routine that tells whether a scalar function of time is decreasing at a "
               "time (GFUDS, ZZGFRELX); SPICELIB's examples use UDDC.",
        "args": [("udfunc", "procedure(gf_scalar)", "", "", "the scalar function"),
                 ("et", "real(wp)", "in", "", "the time (TDB seconds past J2000)"),
                 ("xbool", "logical", "out", "", "the function is decreasing at ET")]},
    "gf_scalar": {
        "kind": "subroutine",
        "doc": "A user-defined scalar function of time for GF (GFUDS, GFUDB, UDDF, UDDC).",
        "args": [("et", "real(wp)", "in", "", "the time (TDB seconds past J2000)"),
                 ("value", "real(wp)", "out", "", "the value of the function")]},
}

# Dummy procedures (EXTERNAL arguments): routine -> {argument: its callback
# (CALLBACKS)}.
PROCEDURE_ARGS = {
    "zzdsksns": {"cmpfun": "zzdsk_compare"},
    **{r: {a: "zzspk_state"} for r, a in (("zzspkflt", "trgsub"), ("zzspkfao", "obssub"),
                                          ("zzspkfap", "trgsub"), ("zzspkfat", "trgsub"),
                                          ("zzspkfzo", "obssub"), ("zzspkfzt", "trgsub"))},
    "zzsfxcor": {"udnear": "zzsfx_near", "udmaxr": "zzsfx_maxrad", "udrayx": "zzsfx_rayx"},
    "zztanslv": {"udcond": "zztan_cond", "udstep": "gf_step", "udrefn": "gf_refine"},
    "uddf": {"udfunc": "gf_scalar"},
    "uddc": {"udfunc": "gf_scalar"},
    # GF
    'gfevnt': {'udstep': 'gf_step', 'udrefn': 'gf_refine', 'udrepi': 'gf_rep_init', 'udrepu': 'gf_rep_update', 'udrepf': 'gf_rep_final', 'udbail': 'gf_bail'},
    'gffove': {'udstep': 'gf_step', 'udrefn': 'gf_refine', 'udrepi': 'gf_rep_init', 'udrepu': 'gf_rep_update', 'udrepf': 'gf_rep_final', 'udbail': 'gf_bail'},
    'gfocce': {'udstep': 'gf_step', 'udrefn': 'gf_refine', 'udrepi': 'gf_rep_init', 'udrepu': 'gf_rep_update', 'udrepf': 'gf_rep_final', 'udbail': 'gf_bail'},
    'zzgfcslv': {'udstep': 'gf_step', 'udrefn': 'gf_refine', 'udrepi': 'gf_rep_init', 'udrepu': 'gf_rep_update', 'udrepf': 'gf_rep_final', 'udbail': 'gf_bail'},
    'zzgflong': {'udstep': 'gf_step', 'udrefn': 'gf_refine', 'udrepi': 'gf_rep_init', 'udrepu': 'gf_rep_update', 'udrepf': 'gf_rep_final', 'udbail': 'gf_bail'},
    'gfudb': {'udfuns': 'gf_scalar', 'udfunb': 'gf_boolean'},
    'gfuds': {'udfuns': 'gf_scalar', 'udqdec': 'gf_decr_x'},
    'zzgfrel': {'udstep': 'gf_step', 'udrefn': 'gf_refine', 'udqdec': 'gf_decr', 'udcond': 'gf_cond', 'udfunc': 'gf_scalar', 'udqref': 'gf_ref', 'udrepi': 'gf_rep_init', 'udrepu': 'gf_rep_update', 'udrepf': 'gf_rep_final', 'udbail': 'gf_bail'},
    'zzgfrelx': {'udstep': 'gf_step', 'udrefn': 'gf_refine', 'udqdec': 'gf_decr_x', 'udcond': 'gf_boolean', 'udfunc': 'gf_scalar', 'udrepi': 'gf_rep_init', 'udrepu': 'gf_rep_update', 'udrepf': 'gf_rep_final', 'udbail': 'gf_bail'},
    'zzgfsolv': {'udcond': 'gf_cond', 'udstep': 'gf_step', 'udrefn': 'gf_refine', 'udbail': 'gf_bail', 'udrepu': 'gf_rep_update'},
    'zzgfsolvx': {'udfuns': 'gf_scalar', 'udfunb': 'gf_boolean', 'udstep': 'gf_step', 'udrefn': 'gf_refine', 'udbail': 'gf_bail', 'udrepu': 'gf_rep_update'},
    'zzgfudb': {'udfuns': 'gf_scalar', 'udfunb': 'gf_boolean', 'udstep': 'gf_step', 'udrefn': 'gf_refine', 'udrepi': 'gf_rep_init', 'udrepu': 'gf_rep_update', 'udrepf': 'gf_rep_final', 'udbail': 'gf_bail'},
    'zzgfcodc': {'udfunc': 'gf_scalar'},
    'zzgfcoex': {'udfunc': 'gf_scalar'},
    'zzgfcocd': {'udfunc': 'gf_scalar'},
    'zzgfcosd': {'udfunc': 'gf_scalar'},
    'zzgfdidc': {'udfunc': 'gf_scalar'},
    'zzgfildc': {'udfunc': 'gf_scalar'},
    'zzgfpadc': {'udfunc': 'gf_scalar'},
    'zzgfrrdc': {'udfunc': 'gf_scalar'},
    'zzgfspdc': {'udfunc': 'gf_scalar'},
    'zzgfudlt': {'udfunc': 'gf_scalar'},
}

# Descriptions of the state components (the !! comments of the generated
# state types), for the umbrellas without "docs" in UMBRELLAS.
STATE_DOCS = {
    'spkbsr': {'nft': 'number of loaded files', 'fthan': 'handle of each loaded file (in load order)', 'ftnum': 'file number of each loaded file: its place in the load (and search) order', 'next': 'the file number of the file loaded last', 'nbt': 'number of bodies in the body table', 'btbeg': 'head of the segment list of each body', 'btbod': "each body's code", 'btexp': 'expense (number of segment descriptors examined) of the segment list of each body', 'bthfs': 'highest file number searched for the segment list of each body', 'btlfs': 'lowest file number searched for the segment list of each body', 'btlb': 'lower bound of the re-use interval of the segment returned last for each body', 'btub': 'upper bound of the re-use interval of the segment returned last for each body', 'btprvd': 'descriptor of the segment returned last for each body', 'btprvi': 'identifier of the segment returned last for each body', 'btprvh': 'handle of the file of the segment returned last for each body', 'btchkp': 'the segment returned last for each body is to be checked first (its re-use interval is valid)', 'btruex': 'expense of the re-use interval of each body', 'sthan': 'handle of the file of each segment in the segment table', 'stpool': 'doubly linked list pool of the segment table (one list per body, in priority order)', 'stdes': 'descriptor of each segment in the segment table', 'stidnt': 'identifier of each segment in the segment table'},
    'pckbsr': {'nft': 'number of loaded files', 'fthan': 'handle of each loaded file (in load order)', 'ftnum': 'file number of each loaded file: its place in the load (and search) order', 'next': 'the file number of the file loaded last', 'nbt': 'number of bodies in the body table', 'btbeg': 'head of the segment list of each body', 'btbod': "each body's code", 'btexp': 'expense (number of segment descriptors examined) of the segment list of each body', 'bthfs': 'highest file number searched for the segment list of each body', 'btlfs': 'lowest file number searched for the segment list of each body', 'btlb': 'lower bound of the re-use interval of the segment returned last for each body', 'btub': 'upper bound of the re-use interval of the segment returned last for each body', 'btprvd': 'descriptor of the segment returned last for each body', 'btprvi': 'identifier of the segment returned last for each body', 'btprvh': 'handle of the file of the segment returned last for each body', 'btchkp': 'the segment returned last for each body is to be checked first (its re-use interval is valid)', 'btruex': 'expense of the re-use interval of each body', 'sthan': 'handle of the file of each segment in the segment table', 'stpool': 'doubly linked list pool of the segment table (one list per body, in priority order)', 'stdes': 'descriptor of each segment in the segment table', 'stidnt': 'identifier of each segment in the segment table'},
    'ckbsr': {'nft': 'number of loaded files', 'fthan': 'handle of each loaded file (in load order)', 'ftnum': 'file number of each loaded file: its place in the load (and search) order', 'next': 'the file number of the file loaded last', 'nit': 'number of instruments in the instrument table', 'itbeg': 'head of the segment list of each instrument', 'itexp': 'expense (number of segment descriptors examined) of the segment list of each instrument', 'ithfs': 'highest file number searched for the segment list of each instrument', 'itlfs': 'lowest file number searched for the segment list of each instrument', 'itlb': 'lower bound of the re-use interval of the segment returned last for each instrument', 'itub': 'upper bound of the re-use interval of the segment returned last for each instrument', 'itprvd': 'descriptor of the segment returned last for each instrument', 'itprvi': 'identifier of the segment returned last for each instrument', 'itprvh': 'handle of the file of the segment returned last for each instrument', 'itchkp': 'the segment returned last for each instrument is to be checked first (its re-use interval is valid)', 'itruex': 'expense of the re-use interval of each instrument', 'sthan': 'handle of the file of each segment in the segment table', 'stpool': 'doubly linked list pool of the segment table (one list per instrument, in priority order)', 'itins': 'each spacecraft clock/instrument code', 'itprvf': 'angular velocity flag of the segment returned last for each instrument (non-zero: present)', 'stidnt': 'identifier of each segment in the segment table', 'stdcd': 'double precision components of the descriptor of each segment', 'sticd': 'integer components of the descriptor of each segment', 'findex': 'index in the file table of the file being searched', 'iindex': 'index in the instrument table of the instrument being searched', 'fnd': 'a segment was found by the last search', 'fresub': "the partial list left by the 'CHECK PARTIAL LIST' state must be freed", 'alpha': 'start of the interval of interest (request time minus tolerance) of the search', 'omega': 'end of the interval of interest (request time plus tolerance) of the search', 'reqt': 'the request time (encoded spacecraft clock) of the search', 'savtol': 'the time tolerance of the search', 'scinst': 'the instrument of the search', 'avneed': 'the search requires angular velocity', 'newsch': 'the search is new (CKBSS was called; CKSNS not yet)', 'status': 'the state of the search in progress', 'savep': 'the segment list node to continue the search from', 'slbeg': 'head of the partial segment list being checked', 'top': 'depth of the stack of interrupted states of the search'},
    'zzdskbsr': {'nft': 'number of loaded files', 'fthan': 'handle of each loaded file (in load order)', 'ftnum': 'file number of each loaded file: its place in the load (and search) order', 'next': 'the file number of the file loaded last', 'nbt': 'number of bodies in the body table', 'btbeg': 'head of the segment list of each body', 'btbod': "each body's code", 'btexp': 'expense (number of segment descriptors examined) of the segment list of each body', 'bthfs': 'highest file number searched for the segment list of each body', 'btlfs': 'lowest file number searched for the segment list of each body', 'sthan': 'handle of the file of each segment in the segment table', 'stpool': 'doubly linked list pool of the segment table (one list per body, in priority order)', 'stdlad': 'DLA descriptor of each segment in the segment table', 'stdskd': 'DSK descriptor of each segment in the segment table', 'dskctr': 'counter of changes of the loaded DSK set (checked by ZZDSKCHK)', 'pass1': 'the counter has not been set up yet', 'begsch': 'a search was begun by ZZDSKBSS', 'findex': 'index in the file table of the file being searched', 'bindex': 'index in the body table of the body being searched', 'fnd': 'a segment was found by the last search', 'svbody': 'the body of the search', 'status': 'the state of the search in progress', 'savep': 'the segment list node to continue the search from', 'top': 'depth of the stack of interrupted states of the search'},
    'dasa2l': {
        'fidx': 'index (in the tables) of the file looked up last',
        'next': 'the next data type (character, double precision, integer, cyclic)',
        'prev': 'the previous data type (cyclic)',
        'nfiles': 'number of files in the tables',
        'nw': 'words per record of each data type',
        'prvhan': 'handle of the file looked up last',
        'rngloc': "location of each data type's address range in a directory record",
        'tbbase': 'first record of the single cluster of each data type, for each file',
        'tbfwrd': 'forward pointer of the first directory record of each file',
        'tbhan': 'handles of the files in the tables',
        'tbmxad': 'largest address of each data type, for each file',
        'tbsize': 'number of records of the single cluster of each data type, for each file',
        'fast': 'the file looked up last has one cluster per data type (fast lookup)',
        'prvok': 'the last lookup was valid',
        'tbfast': 'each file has one cluster per data type (fast lookup)',
        'tbrdon': 'each file is open for reading only (so its tables do not change)',
    },
    'dasrwr': {
        'rcbufc': 'the buffered character records',
        'rcbufd': 'the buffered double precision records',
        'rcbufi': 'the buffered integer records',
        'rnbufc': 'record number of each buffered character record',
        'rnbufd': 'record number of each buffered double precision record',
        'rnbufi': 'record number of each buffered integer record',
        'hnbufc': 'file handle of each buffered character record',
        'hnbufd': 'file handle of each buffered double precision record',
        'hnbufi': 'file handle of each buffered integer record',
        'upbufc': 'each buffered character record was updated and is not written to the file yet',
        'upbufd': 'each buffered double precision record was updated and is not written to the file yet',
        'upbufi': 'each buffered integer record was updated and is not written to the file yet',
        'poolc': 'linked-list pool of the character buffer (most recently used first)',
        'poold': 'linked-list pool of the double precision buffer (most recently used first)',
        'pooli': 'linked-list pool of the integer buffer (most recently used first)',
        'headc': "head of the list of the character buffer's entries",
        'headd': "head of the list of the double precision buffer's entries",
        'headi': "head of the list of the integer buffer's entries",
        'usedc': 'number of entries of the character buffer in use',
        'usedd': 'number of entries of the double precision buffer in use',
        'usedi': 'number of entries of the integer buffer in use',
        'pass1': 'the buffers have not been initialized yet',
    },
    'dasec': {
        'crecrd': 'the comment record read last',
        'filchr': 'number of comment characters read from each file',
        'filhan': 'handles of the files whose comments are being read',
        'lsthan': 'the handle read last',
        'lstpos': 'position in the last comment record of each file',
        'lstrec': 'the comment record read last of each file',
        'nfiles': 'number of files whose comments are being read',
        'first': 'the tables have not been set up yet (they are set up on the first call)',
    },
    'dasadc': {
        'record': 'work buffer of one character record (saved in SPICELIB to keep it off the stack)',
    },
    'dasadd': {
        'record': 'work buffer of one double precision record (saved in SPICELIB to keep it off the stack)',
    },
    'dasadi': {
        'record': 'work buffer of one integer record (saved in SPICELIB to keep it off the stack)',
    },
    'dascls': {
        'fhset': 'set of the handles of the open DAS files (sized once)',
        'pass1': 'the handle set has not been sized yet',
    },
    'wrencc': {
        'intesc': 'character code of the escape character',
        'intfpc': 'character code of the first printable character',
        'intlpc': 'character code of the last printable character',
        'intquo': 'character code of the quote character',
        'first': 'the tables have not been set up yet (they are set up on the first call)',
    },
    'zzdasgrd': {
        'first': 'the tables have not been set up yet (they are set up on the first call)',
        'intbff': 'code of the binary file format of the file read last',
        'natbff': 'code of the native binary file format',
    },
    'zzdasgri': {
        'first': 'the tables have not been set up yet (they are set up on the first call)',
        'intbff': 'code of the binary file format of the file read last',
        'natbff': 'code of the native binary file format',
    },
    'zzdasnfr': {
        'first': 'the tables have not been set up yet (they are set up on the first call)',
        'ftpstr': 'the FTP validation string',
        'prenul': 'the bytes before the FTP validation string (nulls)',
        'pstnul': 'the bytes after the FTP validation string (nulls)',
    },
    'zzdasrfr': {
        'first': 'the tables have not been set up yet (they are set up on the first call)',
        'natbff': 'code of the native binary file format',
    },
    'dskd02': {
        'first': 'no segment has been read yet',
        'prvhan': 'handle of the DSK file of the segment read last',
        'prvbas': 'DLA base address of the segment read last',
        'nv': 'number of vertices of the segment read last',
    },
    'dski02': {
        'first': 'no segment has been read yet',
        'prvhan': 'handle of the DSK file of the segment read last',
        'prvbas': 'DLA base address of the segment read last',
        'cgscal': 'coarse voxel scale of the segment read last',
        'np': 'number of plates of the segment read last',
        'nv': 'number of vertices of the segment read last',
        'nvxtot': 'number of voxels of the segment read last',
        'voxnpl': 'size of the voxel-plate pointer array of the segment read last',
        'voxnpt': 'size of the voxel-plate correspondence list of the segment read last',
        'vtxnpl': 'size of the vertex-plate correspondence list of the segment read last',
    },
    'bodvrd': {
        'svbdnm': 'the body name translated last',
        'svbdid': 'its NAIF ID code',
        'svfnd1': 'the name was found',
        'first': 'no name has been translated yet',
    },
    'dafana': {
        'stifnm': 'internal file name of each DAF being written',
        'staddg': 'an array is being added to each DAF (between DAFBNA and DAFENA)',
        'stfrst': 'record number of the first summary record of each DAF',
        'stlast': 'record number of the last summary record of each DAF',
        'stfree': 'address of the next free word of each DAF',
        'stbegn': 'beginning address of the new array of each DAF',
        'stlsum': 'summary of the new array of each DAF',
        'stname': 'name of the new array of each DAF',
        'stfh': 'handle of each DAF being written',
        'stpool': 'linked-list pool of the state table (active and free entries)',
        'sthead': 'head of the list of active entries of the state table',
        'stfptr': 'head of the list of free entries of the state table',
        'first': 'the state table has not been initialized yet',
    },
    'daffa': {
        'stfh': 'handle of each DAF being searched',
        'stprev': 'record number of the previous summary record of each search',
        'stthis': 'record number of the current summary record of each search',
        'stnext': 'record number of the next summary record of each search',
        'stnseg': 'number of summaries in the current summary record of each search',
        'stcurr': 'index of the current summary within its record, for each search',
        'stnr': 'the name record read last, for each search',
        'sthvnr': 'the name record of the current array is buffered (stnr), for each search',
        'stsr': 'the summary record read last, for each search',
        'stpool': 'linked-list pool of the state table (active and free entries)',
        'sthead': 'head of the list of active entries of the state table',
        'stfptr': 'head of the list of free entries of the state table',
        'first': 'the state table has not been initialized yet',
    },
    'dafec': {
        'crecrd': 'the comment record read last',
        'eocmrk': 'the end-of-comments marker',
        'eolmrk': 'the end-of-line marker',
        'filcnt': 'number of comment lines read from each file',
        'filhan': 'handles of the files whose comments are being read',
        'lsthan': 'the handle read last',
        'lstpos': 'position in the last comment record of each file',
        'lstrec': 'the comment record read last of each file',
        'nfiles': 'number of files whose comments are being read',
        'first': 'the tables have not been set up yet (they are set up on the first call)',
    },
    'dafrwd': {
        'rbhan': 'handle of the file of each buffered record',
        'rbrec': 'record number of each buffered record',
        'rbreq': 'request number of the last use of each buffered record (to replace the oldest)',
        'rbdat': 'the buffered records',
        'rbnbr': 'number of records in the buffer',
        'nread': 'number of records read from files',
        'nreq': 'number of requests',
    },
    'dafwda': {
        'buffer': 'work buffer of one record (saved in SPICELIB to keep it off the stack)',
    },
    'hx2dp': {
        'maxdp': 'largest number divided by the base',
        'mindp': 'smallest number divided by the base',
        'scales': 'negative powers of the base (16) for the digits of the mantissa',
        'digbeg': "character code of '0'",
        'digend': "character code of '9'",
        'iexpch': 'character code of the exponent character',
        'iminus': "character code of '-'",
        'iplus': "character code of '+'",
        'ispace': "character code of ' '",
        'lccbeg': "character code of 'a'",
        'lccend': "character code of 'f'",
        'uccbeg': "character code of 'A'",
        'uccend': "character code of 'F'",
        'first': 'the tables have not been set up yet (they are set up on the first call)',
    },
    'zzbodblt': {
        'bltnam': 'names of the built-in bodies',
        'bltnor': 'the names, normalized',
        'bltcod': 'NAIF ID codes of the built-in bodies',
        'first': 'the tables have not been set up yet (they are set up on the first call)',
    },
    'zzbodtrn': {
        'defnam': 'names of the built-in and BODDEF-defined bodies',
        'defnor': 'the names, normalized (uppercase, single blanks)',
        'defcod': 'their NAIF ID codes',
        'defsiz': 'number of built-in and defined names',
        'dnmlst': 'hash table of the defined names: head nodes',
        'dnmpol': 'hash table of the defined names: collision lists',
        'dnmnms': 'hash table of the defined names: the names',
        'dnmidx': 'hash table of the defined names: index of each name in defnam',
        'didlst': 'hash table of the defined codes: head nodes',
        'didpol': 'hash table of the defined codes: collision lists',
        'didids': 'hash table of the defined codes: the codes',
        'dididx': 'hash table of the defined codes: index of each code in defcod',
        'kernam': 'names from the kernel pool (NAIF_BODY_NAME)',
        'kercod': 'codes from the kernel pool (NAIF_BODY_CODE)',
        'knmlst': 'hash table of the kernel pool names: head nodes',
        'knmpol': 'hash table of the kernel pool names: collision lists',
        'knmnms': 'hash table of the kernel pool names: the names',
        'knmidx': 'hash table of the kernel pool names: index of each name in kernam',
        'kidlst': 'hash table of the kernel pool codes: head nodes',
        'kidpol': 'hash table of the kernel pool codes: collision lists',
        'kidids': 'hash table of the kernel pool codes: the codes',
        'kididx': 'hash table of the kernel pool codes: index of each code in kercod',
        'subctr': 'body name state counter (changes when the name tables change)',
        'pulctr': 'the pool state counter when the kernel pool tables were built',
        'bodchg': 'the defined names changed',
        'extker': 'the kernel pool defines names',
        'nodata': 'the kernel pool tables have not been built yet',
        'i': 'work variable (saved in SPICELIB; not shown to carry a value between calls)',
        'first': 'the tables have not been set up yet (they are set up on the first call)',
    },
    'zzdafgdr': {
        'first': 'the tables have not been set up yet (they are set up on the first call)',
        'natbff': 'code of the native binary file format',
        'strbff': 'names of the binary file formats',
    },
    'zzdafgfr': {
        'first': 'the tables have not been set up yet (they are set up on the first call)',
        'natbff': 'code of the native binary file format',
        'strbff': 'names of the binary file formats',
    },
    'zzdafgsr': {
        'first': 'the tables have not been set up yet (they are set up on the first call)',
        'natbff': 'code of the native binary file format',
        'strbff': 'names of the binary file formats',
    },
    'zzdafnfr': {
        'first': 'the tables have not been set up yet (they are set up on the first call)',
        'ftpstr': 'the FTP validation string',
        'prenul': 'the bytes before the FTP validation string (nulls)',
        'pstnul': 'the bytes after the FTP validation string (nulls)',
    },
    'zzddhgsd': {
        'clslst': 'names of the classes of strings (METHOD, ARCH, BFF)',
        'stramh': 'names of the access methods',
        'strarc': 'names of the architectures',
        'strbff': 'names of the binary file formats',
    },
    'zzddhmnm': {
        'first': 'the tables have not been set up yet (they are set up on the first call)',
        'natbff': 'code of the native binary file format',
        'supbff': 'codes of the supported binary file formats',
        'numsup': 'number of supported binary file formats',
    },
    'zzddhnfc': {
        'first': 'the tables have not been set up yet (they are set up on the first call)',
        'savbff': 'code of the native binary file format',
        'strbff': 'names of the binary file formats',
    },
    'zzddhppf': {
        'first': 'the tables have not been set up yet (they are set up on the first call)',
        'ftpdlm': 'delimiter of the FTP validation string',
        'ftplft': 'left bracket of the FTP validation string',
        'ftpmem': 'the FTP validation string without its brackets',
        'ftprgt': 'right bracket of the FTP validation string',
        'null': 'the null character',
        'strarc': 'names of the architectures',
        'strbff': 'names of the binary file formats',
    },
    'zzftpchk': {
        'first': 'the tables have not been set up yet (they are set up on the first call)',
        'lftbkt': 'left bracket of the FTP validation string',
        'memstr': 'the FTP validation string without its brackets',
        'rgtbkt': 'right bracket of the FTP validation string',
    },
    'zzftpstr': {
        'first': 'the tables have not been set up yet (they are set up on the first call)',
        'locdlm': 'delimiter of the pieces of the FTP validation string',
        'loclnd': 'left bracket of the FTP validation string',
        'locrnd': 'right bracket of the FTP validation string',
        'locstr': 'the FTP validation string',
    },
    'zzplatfm': {
        'first': 'the tables have not been set up yet (they are set up on the first call)',
        'attcpy': 'copy of the attribute names (uppercase)',
        'keyval': 'value of each platform attribute (system, compiler, format, ...)',
    },
    'zzxlated': {
        'bigint': 'largest integer',
        'smlint': 'smallest integer',
        'strbff': 'names of the binary file formats',
        'natbff': 'code of the native binary file format',
        'first': 'the tables have not been set up yet (they are set up on the first call)',
    },
    'zzxlatei': {
        'first': 'the tables have not been set up yet (they are set up on the first call)',
        'natbff': 'code of the native binary file format',
        'strbff': 'names of the binary file formats',
        'smlint': 'smallest integer',
        'bigint': 'largest integer',
    },
    'zztime': {
        'rep': 'the token representation of the time string: one letter per token',
        'size': 'number of tokens',
        'begs': 'first character of each token in the string',
        'ends': 'last character of each token in the string',
        'pbegs': "first character of each token's picture in pictur",
        'pends': "last character of each token's picture in pictur",
        'pictur': 'the format picture (as for TIMOUT) built from the tokens',
        'names': 'name of each token class (by its letter), for error messages',
        'picerr': 'template of the message for a picture that is too long',
        'tknerr': 'template of the message for too many tokens',
        'hms': 'work variable (saved in SPICELIB; not shown to carry a value between calls); hours, minutes and seconds',
        'blank': "character code of ' '",
        'to': 'work variable (saved in SPICELIB; not shown to carry a value between calls)',
        'pto': 'work variable (saved in SPICELIB; not shown to carry a value between calls)',
        'case': 'work variable (saved in SPICELIB; not shown to carry a value between calls)',
        'mnsize': 'number of characters of month names (full, short)',
        'wksize': 'number of characters of weekday names (full, short)',
        'width': 'length of each recognized substring',
        'recog': 'the recognized substrings (month and weekday names, eras, ...)',
        'mnmrk': 'picture markers of month names by case (lower, upper, mixed) and length',
        'wkday': 'picture markers of weekday names by case and length',
        'class': 'token class letter of each recognized substring',
        'f': 'first recognized substring that starts with each character',
        'l': 'last recognized substring that starts with each character',
        'ampm': 'the string has an AM/PM marker',
        'first': 'the tables have not been set up yet (they are set up on the first call)',
    },
}
