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
    "pepper_keeper": """
        inttxt intord repmot sepool stpool tkvrsn zzascii exists zzldker keeper
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
ALLOWED_LEGACY = {
    "eklef": "loads an EK file (the EK query manager EKQMGR moves with the EK subsystem, phase 6)",
    "ekuef": "unloads an EK file (the EK query manager EKQMGR moves with the EK subsystem, phase 6)",
    "wrline": "writes a line to a device or file (SPICELIB's error output; it moves "
              "with the error output, phase 7)",
}

# Dummy procedures (EXTERNAL arguments): routine -> {argument: the abstract
# interface in pepper_instance that declares it}.
PROCEDURE_ARGS = {
    "zzdsksns": {"cmpfun": "zzdsk_compare"},
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
