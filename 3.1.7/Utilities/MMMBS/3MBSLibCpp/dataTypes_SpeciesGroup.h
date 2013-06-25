#ifndef DATATYPES_SPECIESGROUP_H
#define DATATYPES_SPECIESGROUP_H

#include <windows.h>
#include <string.h>
#include <stdio.h>
#include <tchar.h>
#include "params.h"

//--------------------------------------------------------------------------------------//
// Species groups
//---------------//

const int SZGROUPTNAMEBUFFERLENGTH = SIZE_32;

// Limit SZSPECIESGROUPNAMEBUFFER to 15 characters for formating reasons when printing takes.  If length changes, must
// visit extraction routine to account for change in buffer length.
//const TCHAR SZSPECIESGROUPNAMEBUFFER[SIZE_16][SZGROUPTNAMEBUFFERLENGTH] = {"Mysticete", TEXT("HF Odontocete", TEXT("MF Odontocete", TEXT("Special Consid.", TEXT("Phocid Phocinae", TEXT("Otariid", TEXT("Other Mammals", TEXT("Sea Turtles", TEXT("Phocid Monachinae"};
//const TCHAR SZSPECIESGROUPNAMEBUFFER[SIZE_16][SZGROUPTNAMEBUFFERLENGTH] = {"Mysticete", TEXT("HF Odontocete", TEXT("MF Odontocete", TEXT("P. Phocoena", TEXT("Phocid Phocinae", TEXT("Otariid", TEXT("Other Mammals", TEXT("Sea Turtles", TEXT("Phocid Monachinae"};
//enum SPECIESGROUP{MYSTICETE, HFODONTOCETE, MFODONTOCETE, SPECIALCONSIDRTNS, PHOCID_PHOCINAE, OTARIID, OTHERMARINEMAMMALS, SEATURTLES, PHOCID_MONACHINAE};

const TCHAR SZSPECIESGROUPNAMEBUFFER[NUM_SPEGROUPS_INUSE][SZGROUPTNAMEBUFFERLENGTH] = {TEXT("Mysticete"),
																					   TEXT("HF Odontocete"),
																					   TEXT("MF Odontocete"),
																					   TEXT("Phocid (Monachinae)"),
																					   TEXT("Phocid (Phocinae)"),
																					   TEXT("Otariid"),
																					   TEXT("Other Mammals"),
																					   TEXT("Sea Turtles"),
																					   TEXT("P. Phocoena") /*P. Phocoena is Special Consid.*/,
																					   TEXT("Sound Source")};
enum SPECIESGROUP{MYSTICETE, HFODONTOCETE, MFODONTOCETE, PHOCID_MONACHINAE, PHOCID_PHOCINAE, OTARIID, OTHERMARINEMAMMALS, SEATURTLES, SPECIALCONSIDRTNS, SOUNDSOURCE};


enum SPECIESNAME
{

	// ODONTOCETES (HF SPECIALIST)
	//		GENERIC
	GENERIC_ODONTOCETES_HF,

	//		FAMILY PHOCOENIDAE   		
	PHOCOENA_SPINIPINNIS, PHOCOENA_SINUS, NEOPHOCAENA_PHOCAENOIDES, AUSTRALOPHOCAENA_DIOPTRICA, PHOCOENOIDES_DALLI,
	OPEN_6, OPEN_7, OPEN_8,

	//		FAMILY_DELPHINIDAE,
	CEPHALORHYNCHUS_COMMERSONII, CEPHALORHYNCHUS_EUTROPIA, CEPHALORHYNCHUS_HEAVISIDII,
	CEPHALORHYNCHUS_HECTORI, OPEN_13, OPEN_14, OPEN_15,

	//		FAMILY KOGIIDAE		
	KOGIA_BREVICEPS, KOGIA_SIMUS, OPEN_18, OPEN_19,

	//		FAMILY PLATANISTIDAE		
	PLATANISTA_GANGETICA, PLATANISTA_MINOR, OPEN_22, OPEN_23,

	//		FAMILY PONTOPORIIDAE		
	LIPOTES_VEXILLIFER, PONTOPORIA_BLAINVILLEI, OPEN_26, OPEN_27,

	//		FAMILY INIIDAE		
	INIA_GEOFFRENSIS, OPEN_29,

	//	ODONTOCETES (MF SPECIALIST)
	//		GENERIC
	GENERIC_ODONTOCETES_MF,

	//		FAMILY DELPHINIDAE		
	STENO_BREDANENSIS, SOUSA_CHINENSIS, SOUSA_PLUMBEA, SOUSA_TEUSZII, SOTALIA_FLUVIATILIS, LAGENORHYNCHUS_ALBIROSTRIS,
	LAGENORHYNCHUS_ACUTUS, LAGENORHYNCHUS_OBSCURUS, LAGENORHYNCHUS_OBLIQUIDENS, LAGENORHYNCHUS_CRUCIGER,
	LAGENORHYNCHUS_AUSTRALIS, GRAMPUS_GRISEUS, TURSIOPS_TRUNCATUS, LAGENODELPHIS_HOSEI, STENELLA_FRONTALIS,
	STENELLA_ATTENUATA, STENELLA_LONGIROSTRIS, STENELLA_CLYMENE, STENELLA_COERULEOALBA, DELPHINUS_DELPHIS,
	DELPHINUS_CAPENSIS, LISSODELPHIS_BOREALIS, LISSODELPHIS_PERONII, PEPONOCEPHALA_ELECTRA, FERESA_ATTENUATA,
	PSEUDORCA_CRASSIDENS, ORCINUS_ORCA, GLOBICEPHALA_MELAS, GLOBICEPHALA_MACRORHYNCHUS, OPEN_60, OPEN_61, OPEN_62,
	OPEN_63, OPEN_64, OPEN_65,

	//		FAMILY ZIPHIIDAE
	TASMACETUS_SHEPHERDI, BERARDIUS_BAIRDII, BERARDIUS_ARNUXII,	MESOPLODON_PACIFICUS, MESOPLODON_BIDENS,
	MESOPLODON_DENSIROSTRIS, MESOPLODON_EUROPAEUS, MESOPLODON_LAYARDII, MESOPLODON_HECTORI, MESOPLODON_GRAYI,
	MESOPLODON_STEJNEGERI, MESOPLODON_BOWDOINI, MESOPLODON_MIRUS, MESOPLODON_GINKGODENS, MESOPLODON_CARLHUBBSI,
	MESOPLODON_PERUVIANUS, ZIPHIUS_CAVIROSTRIS, HYPEROODON_AMPULLATUS, HYPEROODON_PLANIFRONS, OPEN_85, OPEN_86,
	OPEN_87, OPEN_88, OPEN_89,

	//		FAMILY PHYSETERIDAE		
	PHYSETER_MACROCEPHALUS, OPEN_91, OPEN_92,

	//		FAMILY MONODONTIDAE		
	ORCAELLA_BREVIROSTRIS, DELPHINAPTERUS_LEUCAS, MONODON_MONOCEROS, OPEN_96,

	// MYSTICETES (LF SPECIALISTS)			
	//		GENERIC
	GENERIC_MYSTICETES,

	//		FAMILY BALAENIDAE	
	BALAENA_MYSTICETUS, EUBALAENA_AUSTRALIS, EUBALAENA_GLACIALIS, OPEN_101, OPEN_102,

	//		FAMILY BALAENOPTERIDAE
	BALAENOPTERA_ACUTOROSTRATA, BALAENOPTERA_BOREALIS, BALAENOPTERA_EDENI, BALAENOPTERA_MUSCULUS,
	BALAENOPTERA_PHYSALUS, MEGAPTERA_NOVAEANGLIAE, OPEN_109, OPEN_110, 

	//		FAMILY ESCHRICHTIIDAE		
	ESCHRICHTIUS_ROBUSTUS, OPEN_112, OPEN_113,

	//		FAMILY NEOBALAENIDAE		
	CAPEREA_MARGINATA, OPEN_115, 

	//----------------------------------------------------------------------------------//
	// PINNIPED (PHOCID) subfamily PHOCINAE (changed 5-18-2009)
	//------------------//
	//		GENERIC
	GENERIC_PHOCID_PHOCINAE,

	//		SUBFAMILY PHOCINAE
	CYSTOPHORA_CRISTATA, ERIGNATHUS_BARBATUS, HALICHOERUS_GRYPUS, PHOCA_CASPICA, PHOCA_FASCIATA, PHOCA_GROENLANDICA,
	PHOCA_HISPIDA, PHOCA_LARGHA, PHOCA_SIBIRICA, PHOCA_VITULINA, OPEN_127, OPEN_128, OPEN_129, OPEN_130,
	//----------------------------------------------------------------------------------//

	//----------------------------------------------------------------------------------//
	// PINNIPED (PHOCID)subfamily MONACHINAE (changed 5-18-2009)
	//------------------//
	//		SUBFAMILY MONACHINAE
	HYDRURGA_LEPTONYX, LEPTONYCHOTES_WEDDELLI, LOBODON_CARCINOPHAGUS, MIROUNGA_ANGUSTIROSTRIS, MIROUNGA_LEONINA,
	MONACHUS_MONACHUS, MONACHUS_SCHAUINSLANDI, OMMATOPHOCA_ROSSI, OPEN_139, OPEN_140,
	//----------------------------------------------------------------------------------//

	//	PINNIPED (OTARRID)						
	//		GENERIC
	GENERIC_OTARRID,

	//		SUBFAMILY OTARIINAE	
	EUMETOPIAS_JUBATUS, NEOPHOCA_CINEREA, OTARIA_BYRONIA, PHOCARCTOS_HOOKERI, ZALOPHUS_CALIFORNIANUS, OPEN_147,
	OPEN_148, OPEN_149,

	//		SUBFAMILY ARCTOCEPHALINAE					
	ARCTOCEPHALUS_AUSTRALIS, ARCTOCEPHALUS_FORSTERI, ARCTOCEPHALUS_GALAPAGOENSIS, ARCTOCEPHALUS_GAZELLA,
	ARCTOCEPHALUS_PHILIPPII, ARCTOCEPHALUS_PUSILLUS, ARCTOCEPHALUS_TOWNSENDI, ARCTOCEPHALUS_TROPICALIS,
	CALLORHINUS_URSINUS, OPEN_159, OPEN_160, OPEN_161,

	//	SPECIAL CONSIDERATIONS						
	//		GENERIC
	GENERIC_SPECIALCONSID,

	//		FAMILY PHOCOENIDAE
	PHOCOENA_PHOCOENA, OPEN_164,

	//	OTHER MARINE MAMMALS
	//		GENERIC
	GENERIC_MARINEMAMMAL,

	//		PINNIPEDS					
	//			FAMILY ODOBENIDAE				
	ODOBENUS_ROSMARUS, OPEN_167, OPEN_168,

	//		SIRENIANS					
	//			FAMILY DUGONGIDAE				
	DUGONG_DUGON, OPEN_170, OPEN_171,

	//			FAMILY TRICHECH	IDAE	
	TRICHECHUS_MANATUS, TRICHECHUS_INUNGUIS, TRICHECHUS_SENEGALENSIS, OPEN_175, OPEN_176,

	//		CARNIVORES				
	//			FAMILY MUSTELLIDAE			
	ENHYDRA_LUTRIS, LUTRA_FELINA, OPEN_179, OPEN_180,

	//			FAMILY URSIDAE			
	URSUS_MARITIMUS, OPEN_182,

	//	SEA TURTLES
	GENERIC_SEATURTLE,
	CHELONIA_MYDAS,	CARETTA_CARETTA, LEPIDOCHELYS_KEMPII, LEPIDOCHELYS_OLIVACEA, ERETMOCHELYS_IMBRICATA,
	DERMOCHELYS_CORIACEA, OPEN_190, OPEN_191, OPEN_192,

	// SOUND SOURCES
	SOUNDSOURCE_ALPHA, OPEN_194, OPEN_195, OPEN_196, OPEN_197, OPEN_198, OPEN_199, 

	// FUTURE
	OPEN_200, OPEN_201, OPEN_202, OPEN_203, OPEN_204, OPEN_205, OPEN_206, OPEN_207, OPEN_208, OPEN_209,
	OPEN_210, OPEN_211, OPEN_212, OPEN_213, OPEN_214, OPEN_215, OPEN_216, OPEN_217, OPEN_218, OPEN_219, 
	OPEN_220, OPEN_221, OPEN_222, OPEN_223, OPEN_224, OPEN_225, OPEN_226, OPEN_227, OPEN_228, OPEN_229, 
	OPEN_230, OPEN_231, OPEN_232, OPEN_233, OPEN_234, OPEN_235, OPEN_236, OPEN_237, OPEN_238, OPEN_239,

};



const int SPECIESNAMEBUFFLEN_LATIN = SIZE_32;
const int SPECIESNAMEBUFFLEN_ENGLISH = SIZE_64;
const int SPECIESNAMELENGTH = 240;

const TCHAR SZSPECIESNAMEBUFFER_LATIN[SPECIESNAMELENGTH][SPECIESNAMEBUFFLEN_LATIN] = 
{
	//----------------------------//
	// Odontocetes (HF specialist)
	//----------------------------//
	//		Generic
	TEXT("-------"),

	//		Family Phocoenidae  
	TEXT("Phocoena spinipinnis"), TEXT("Phocoena sinus"), TEXT("Neophocaena phocaenoides"),
	TEXT("Australophocaena dioptrica"), TEXT("Phocoenoides dalli"), TEXT("open6"), TEXT("open7"), TEXT("open8"),

	//		Family Delphinidae
	TEXT("Cephalorhynchus commersonii"), TEXT("Cephalorhynchus eutropia"), TEXT("Cephalorhynchus heavisidii"),
	TEXT("Cephalorhynchus hectori"), TEXT("open13"), TEXT("open14"), TEXT("open15"),

	//		Family Kogiidae
	TEXT("Kogia breviceps"), TEXT("Kogia simus"), TEXT("open18"), TEXT("open19"),

	//		Family Platanistidae
	TEXT("Platanista gangetica"), TEXT("Platanista minor"), TEXT("open22"), TEXT("open23"),

	//		Family Pontoporiidae
	TEXT("Lipotes vexillifer"), TEXT("Pontoporia blainvillei"), TEXT("open26"), TEXT("open27"),

	//		Family Iniidae
	TEXT("Inia geoffrensis"), TEXT("open29"),

	//----------------------------//
	// Odontocetes (MF specialist)
	//----------------------------//
	//		Generic
	TEXT("-------"),

	//		Family Delphinidae
	TEXT("Steno bredanensis"), TEXT("Sousa chinensis"), TEXT("Sousa plumbea"), TEXT("Sousa teuszii"),
	TEXT("Sotalia fluviatilis"), TEXT("Lagenorhynchus albirostris"), TEXT("Lagenorhynchus acutus"), TEXT("Lagenorhynchus obscurus"),
	TEXT("Lagenorhynchus obliquidens"), TEXT("Lagenorhynchus cruciger"), TEXT("Lagenorhynchus australis"), TEXT("Grampus griseus"),
	TEXT("Tursiops truncatus"), TEXT("Lagenodelphis hosei"), TEXT("Stenella frontalis"), TEXT("Stenella attenuata"),
	TEXT("Stenella longirostris"), TEXT("Stenella clymene"), TEXT("Stenella coeruleoalba"), TEXT("Delphinus delphis"),
	TEXT("Delphinus capensis"), TEXT("Lissodelphis borealis"), TEXT("Lissodelphis peronii"),
	TEXT("Peponocephala electra"), TEXT("Feresa attenuata"), TEXT("Pseudorca crassidens"), TEXT("Orcinus orca"),
	TEXT("Globicephala melas"), TEXT("Globicephala macrorhynchus"), TEXT("open60"), TEXT("open61"), TEXT("open62"), TEXT("open63"), TEXT("open64"), TEXT("open65"),

	//		Family Ziphiidae
	TEXT("Tasmacetus shepherdi"), TEXT("Berardius bairdii"), TEXT("Berardius arnuxii"), TEXT("Mesoplodon pacificus"),
	TEXT("Mesoplodon bidens"), TEXT("Mesoplodon densirostris"), TEXT("Mesoplodon europaeus"), TEXT("Mesoplodon layardii"),
	TEXT("Mesoplodon hectori"), TEXT("Mesoplodon grayi"), TEXT("Mesoplodon stejnegeri"), TEXT("Mesoplodon bowdoini"),
	TEXT("Mesoplodon mirus"), TEXT("Mesoplodon ginkgodens"), TEXT("Mesoplodon carlhubbsi"), TEXT("Mesoplodon peruvianus"),
	TEXT("Ziphius cavirostris"), TEXT("Hyperoodon ampullatus"), TEXT("Hyperoodon planifrons"), TEXT("open85"), TEXT("open86"), TEXT("open87"), TEXT("open88"), TEXT("open89"),

	//		Family Physeteridae
	TEXT("Physeter macrocephalus"), TEXT("open91"), TEXT("open92"),

	//		Family Monodontidae
	TEXT("Orcaella brevirostris"), TEXT("Delphinapterus leucas"), TEXT("Monodon monoceros"), TEXT("open96"),


	//----------------------------//
	// Mysticetes (LF specialists)
	//----------------------------//
	//		Generic
	TEXT("-------"),

	//		Family Balaenidae		
	TEXT("Balaena mysticetus"), TEXT("Eubalaena australis"), TEXT("Eubalaena glacialis"),  TEXT("open101"), TEXT("open102"),

	//		Family Balaenopteridae		
	TEXT("Balaenoptera acutorostrata"), TEXT("Balaenoptera borealis"), TEXT("Balaenoptera edeni"), TEXT("Balaenoptera musculus"),
	TEXT("Balaenoptera physalus"), TEXT("Megaptera novaeangliae"), TEXT("open109"), TEXT("open110"),

	//		Family Eschrichtiidae		
	TEXT("Eschrichtius robustus"), TEXT("open112"), TEXT("open113"),

	//		Family Neobalaenidae		
	TEXT("Caperea marginata"), TEXT("open115"),

	//-------------------//
	// Pinniped (Phocid)				
	//-------------------//
	//		Subfamily Phocinae
	//		Generic phocid phocinae
	TEXT("-------"),

	TEXT("Cystophora cristata"), TEXT("Erignathus barbatus"), TEXT("Halichoerus grypus"), TEXT("Phoca  caspica"), TEXT("Phoca fasciata"),
	TEXT("Phoca groenlandica"), TEXT("Phoca hispida"), TEXT("Phoca largha"), TEXT("Phoca sibirica"), TEXT("Phoca vitulina"), TEXT("open127"), TEXT("open128"), TEXT("open129"), TEXT("open130"),

	//		Subfamily Monachinae
	TEXT("Hydrurga leptonyx"), TEXT("Leptonychotes weddelli"), TEXT("Lobodon carcinophagus"), TEXT("Mirounga angustirostris"),
	TEXT("Mirounga leonina"), TEXT("Monachus monachus"), TEXT("Monachus schauinslandi"), TEXT("Ommatophoca rossi"), TEXT("open139"), TEXT("open140"),

	//-------------------//
	// Pinniped (Otarrid)						
	//-------------------//
	//		Generic
	TEXT("-------"),

	//		Subfamily Otariinae					
	TEXT("Eumetopias jubatus"), TEXT("Neophoca cinerea"), TEXT("Otaria byronia"), TEXT("Phocarctos hookeri"), TEXT("Zalophus californianus"),
	TEXT("open147"), TEXT("open148"), TEXT("open149"),

	//		Subfamily Arctocephalinae					
	TEXT("Arctocephalus australis"), TEXT("Arctocephalus forsteri"), TEXT("Arctocephalus galapagoensis"), TEXT("Arctocephalus gazella"),
	TEXT("Arctocephalus philippii"), TEXT("Arctocephalus pusillus"), TEXT("Arctocephalus townsendi"), TEXT("Arctocephalus tropicalis"),
	TEXT("Callorhinus ursinus"), TEXT("open159"), TEXT("open160"), TEXT("open161"),


	//-----------------------//
	// Special Considerations					
	//-----------------------//
	//		Generic
	TEXT("-------"),
	//		Family Phocoenidae   					
	TEXT("Phocoena phocoena"), TEXT("open164"),

	//----------------------//
	//	Other Marine Mammals
	//----------------------//
	//		Generic
	TEXT("-------"),

	//		Pinnipeds					
	//			Family Odobenidae				
	TEXT("Odobenus rosmarus"), TEXT("open167"), TEXT("open168"),

	//		Sirenians					
	//			Family Dugongidae				
	TEXT("Dugong dugon"), TEXT("open170"), TEXT("open171"),

	//			Family Trichechidae			
	TEXT("Trichechus manatus"), TEXT("Trichechus inunguis"), TEXT("Trichechus senegalensis"), TEXT("open175"), TEXT("open176"),

	//		Carnivores
	//			Family Mustellidae			
	TEXT("Enhydra lutris"), TEXT("Lutra felina"), TEXT("open179"), TEXT("open180"),

	//			Family Ursidae		
	TEXT("Ursus maritimus"), TEXT("open182"),

	//--------------//
	//	Sea turtles					
	//--------------//
	//		Generic
	TEXT("-------"),
	TEXT("Chelonia mydas"), TEXT("Caretta caretta"), TEXT("Lepidochelys kempii"), TEXT("Lepidochelys olivacea"), TEXT("Eretmochelys imbricata"),
	TEXT("Dermochelys coriacea"), TEXT("open190"), TEXT("open191"), TEXT("open192"),

	//---------------//
	// Sound Sources
	//---------------//
	TEXT("Sound Source"), TEXT("SS1open194"), TEXT("SS2open195"), TEXT("SS3open196"), TEXT("SS4open197"), TEXT("SS5open198"), TEXT("SS6open199"),


	//-----------//
	// Future Use
	//-----------//
	TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""),
	TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""),
	TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""),
	TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""),
};

const TCHAR SZSPECIESNAMEBUFFER_LATIN_FIRSTINITIAL_SECONDNAME[SPECIESNAMELENGTH][SPECIESNAMEBUFFLEN_LATIN] = 
{
	//----------------------------//
	// Odontocetes (HF specialist)
	//----------------------------//
	//		Generic
	TEXT("generic HF Odontocetes"),

	//		Family Phocoenidae  
	TEXT("P. spinipinnis"), TEXT("P. sinus"), TEXT("N. phocaenoides"), TEXT("A. dioptrica"),
	TEXT("Phocoenoides dalli"), TEXT(""), TEXT(""), TEXT(""),

	//		Family Delphinidae
	TEXT("C. commersonii"), TEXT("C. eutropia"), TEXT("C. heavisidii"), TEXT("C. hectori"),
	TEXT(""), TEXT(""), TEXT(""),

	//		Family Kogiidae
	TEXT("K. breviceps"), TEXT("K. simus"), TEXT(""), TEXT(""),

	//		Family Platanistidae
	TEXT("P. gangetica"), TEXT("P. minor"), TEXT(""), TEXT(""),

	//		Family Pontoporiidae
	TEXT("L. vexillifer"), TEXT("P. blainvillei"), TEXT(""), TEXT(""),

	//		Family Iniidae
	TEXT("I. geoffrensis"), TEXT(""),

	//----------------------------//
	// Odontocetes (MF specialist)
	//----------------------------//
	//		Generic
	TEXT("generic MF Odontocetes"),

	//		Family Delphinidae
	TEXT("S. bredanensis"), TEXT("S. chinensis"), TEXT("S. plumbea"), TEXT("S. teuszii"),
	TEXT("S. fluviatilis"), TEXT("L. albirostris"), TEXT("L. acutus"), TEXT("L. obscurus"),
	TEXT("L. obliquidens"), TEXT("L. cruciger"), TEXT("L. australis"), TEXT("G. griseus"),
	TEXT("T. truncatus"), TEXT("L. hosei"), TEXT("S. frontalis"), TEXT("S. attenuata"),
	TEXT("S. longirostris"), TEXT("S. clymene"), TEXT("S. coeruleoalba"), TEXT("D. delphis"),
	TEXT("D. capensis"), TEXT("L. borealis"), TEXT("L. peronii"),
	TEXT("P. electra"), TEXT("F. attenuata"), TEXT("P. crassidens"), TEXT("O. orca"),
	TEXT("G. melas"), TEXT("G. macrorhynchus"), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""),

	//		Family Ziphiidae
	TEXT("T. shepherdi"), TEXT("B. bairdii"), TEXT("B. arnuxii"), TEXT("M. pacificus"),
	TEXT("M. bidens"), TEXT("M. densirostris"), TEXT("M. europaeus"), TEXT("M. layardii"),
	TEXT("M. hectori"), TEXT("M. grayi"), TEXT("M. stejnegeri"), TEXT("M. bowdoini"),
	TEXT("M. mirus"), TEXT("M. ginkgodens"), TEXT("M. carlhubbsi"), TEXT("M. peruvianus"),
	TEXT("Z. cavirostris"), TEXT("H. ampullatus"), TEXT("H.planifrons"), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""),

	//		Family Physeteridae
	TEXT("P. macrocephalus"), TEXT(""), TEXT(""),

	//		Family Monodontidae
	TEXT("O. brevirostris"), TEXT("D. leucas"), TEXT("M. monoceros"), TEXT(""),


	//----------------------------//
	// Mysticetes (LF specialists)
	//----------------------------//
	//		Generic
	TEXT("generic Mysticetes"),

	//		Family Balaenidae		
	TEXT("B. mysticetus"), TEXT("E. australis"), TEXT("E. glacialis"),  TEXT(""), TEXT(""),

	//		Family Balaenopteridae		
	TEXT("B. acutorostrata"), TEXT("B. borealis"), TEXT("B. edeni"), TEXT("B. musculus"),
	TEXT("B. physalus"), TEXT("M. novaeangliae"), TEXT(""), TEXT(""),

	//		Family Eschrichtiidae		
	TEXT("E. robustus"), TEXT(""), TEXT(""),

	//		Family Neobalaenidae		
	TEXT("C. marginata"), TEXT(""),

	//-------------------//
	// Pinniped (Phocid)				
	//-------------------//
	//		Subfamily Phocinae
	//		Generic
	TEXT("generic Phocid"),

	TEXT("C. cristata"), TEXT("E. barbatus"), TEXT("H. grypus"), TEXT("P. caspica"), TEXT("P. fasciata"),
	TEXT("P. groenlandica"), TEXT("P. hispida"), TEXT("P. largha"), TEXT("P. sibirica"), TEXT("P. vitulina"), TEXT(""), TEXT(""), TEXT(""), TEXT(""),

	//		Subfamily Monachinae			
	TEXT("H. leptonyx"), TEXT("L. weddelli"), TEXT("L. carcinophagus"), TEXT("M. angustirostris"),
	TEXT("M. leonina"), TEXT("M. monachus"), TEXT("M. schauinslandi"), TEXT("O. rossi"), TEXT(""), TEXT(""),

	//-------------------//
	// Pinniped (Otarrid)						
	//-------------------//
	//		Generic
	TEXT("generic Otarrid"),

	//		Subfamily Otariinae					
	TEXT("E. jubatus"), TEXT("N. cinerea"), TEXT("O. byronia"), TEXT("P. hookeri"), TEXT("Z. californianus"),
	 TEXT(""), TEXT(""), TEXT(""),

	//		Subfamily Arctocephalinae					
	TEXT("A. australis"), TEXT("A. forsteri"), TEXT("A. galapagoensis"), TEXT("A. gazella"),
	TEXT("A. philippii"), TEXT("A. pusillus"), TEXT("A. townsendi"), TEXT("A. tropicalis"),
	TEXT("C. ursinus"), TEXT(""), TEXT(""), TEXT(""),


	//-----------------------//
	// Special Considerations					
	//-----------------------//
	//		Generic
	TEXT("generic special"),
	//		Family Phocoenidae   					
	TEXT("P. phocoena"), TEXT(""),

	//----------------------//
	//	Other Marine Mammals
	//----------------------//
	//		Generic
	TEXT("generic other"),

	//		Pinnipeds					
	//			Family Odobenidae				
	TEXT("O. rosmarus"), TEXT(""), TEXT(""),

	//		Sirenians					
	//			Family Dugongidae				
	TEXT("D. dugon"), TEXT(""), TEXT(""),

	//			Family Trichechidae			
	TEXT("T. manatus"), TEXT("T. inunguis"), TEXT("T. senegalensis"), TEXT(""), TEXT(""),

	//		Carnivores
	//			Family Mustellidae			
	TEXT("E. lutris"), TEXT("L. felina"), TEXT(""), TEXT(""),

	//			Family Ursidae		
	TEXT("U. maritimus"), TEXT(""),

	//--------------//
	//	Sea turtles					
	//--------------//
	//		Generic
	TEXT("generic sea turtle"),
	TEXT("C. mydas"), TEXT("C. caretta"), TEXT("L. kempii"), TEXT("L. olivacea"), TEXT("E. imbricata"),
	TEXT("D. coriacea"), TEXT("openSeaTurt1"), TEXT("openSeaTurt3"), TEXT("openSeaTurt3"),

	//---------------//
	// Sound Sources
	//---------------//
	TEXT("Sound Source"), TEXT("yyy"), TEXT("yy"), TEXT("y"), TEXT(""), TEXT(""), TEXT(""),

	//-----------//
	// Future Use
	//-----------//
	TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""),
	TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""),
	TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""),
	TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""),

};

const TCHAR SZSPECIESNAMEBUFFER_ENGLISH[SPECIESNAMELENGTH][SPECIESNAMEBUFFLEN_ENGLISH] =
{

	//----------------------------//
	// Odontocetes (HF specialist)
	//----------------------------//
	//		Generic
	TEXT("generic HF Odontocetes")/*0*/,

	//		Family Phocoenidae  
	TEXT("Burmeister's porpoise") /*1*/, TEXT("vaquita, Gulf of California harbor porpoise")/*2*/, TEXT("finless porpoise")/*3*/,
	TEXT("spectacled porpoise")/*4*/, TEXT("Dall's porpoise")/*5*/, TEXT("")/*6*/, TEXT("")/*7*/, TEXT("")/*8*/,

	//		Family Delphinidae
	TEXT("Commerson's dolphin")/*9*/, TEXT("black dolphin, Chilean dolphin")/*10*/, TEXT("Heaviside's dolphin")/*11*/,
	TEXT("Hector's dolphin")/*12*/, TEXT("")/*13*/, TEXT("")/*14*/, TEXT("")/*15*/,
	
	//		Family Kogiidae
	TEXT("pygmy sperm whale")/*16*/, TEXT("dwarf sperm whale")/*17*/, TEXT("")/*18*/, TEXT("")/*19*/,

	//		Family Platanistidae
	TEXT("Ganges River dolphin, Ganges susu")/*20*/, TEXT("Indus River dolphin, Ganges susu")/*21*/, TEXT("")/*22*/, TEXT("")/*23*/,

	//		Family Pontoporiidae
	TEXT("baiji, Yantze, or Chinese river dolphin")/*24*/, TEXT("franciscana, cachimbo, La Plata dolphin")/*25*/, TEXT("")/*26*/, TEXT("")/*27*/,

	//		Family Iniidae
	TEXT("boto, boutu, bufeo, Amazon river dolphin")/*28*/, TEXT("")/*29*/,


	//----------------------------//
	// Odontocetes (MF specialist)
	//----------------------------//
	//		Generic
	TEXT("generic MF Odontocetes")/*30*/,

	//		Family Delphinidae
	TEXT("rough-toothed dolphin")/*31*/, TEXT("Indo-Pacific hump-backed dolphin")/*32*/, TEXT("plumbeous dolphin")/*33*/,
	TEXT("Atlantic hump-backed dolphin")/*34*/, TEXT("tucuxi")/*35*/, TEXT("white-beaked dolphin")/*36*/,
	TEXT("Atlantic white-sided dolphin")/*37*/, TEXT("dusky dolphin")/*38*/, TEXT("Pacific white-sided dolphin")/*39*/,
	TEXT("hourglass dolphin")/*40*/, TEXT("Peale's dolphin")/*41*/, TEXT("Risso's dolphin")/*42*/, TEXT("bottlenose dolphin")/*43*/,
	TEXT("Fraser's dolphin")/*44*/, TEXT("Atlantic spotted dolphin")/*45*/, TEXT("pantropical spotted dolphin")/*46*/,
	TEXT("spinner dolphin")/*47*/, TEXT("clymene dolphin")/*48*/, TEXT("striped dolphin")/*49*/, TEXT("short-beaked common dolphin")/*50*/,
	TEXT("long-beaked common dolphin")/*51*/, TEXT("northern right whale dolphin")/*52*/, TEXT("southern right whale dolphin")/*53*/,
	TEXT("melon-headed whale, electra dolphin")/*54*/, TEXT("pygmy killer whale")/*55*/, TEXT("false killer whale")/*56*/,
	TEXT("killer whale")/*57*/, TEXT("long-finned pilot whale")/*58*/, TEXT("short-finned pilot whale")/*59*/, TEXT("")/*60*/, TEXT("")/*61*/,
	TEXT("")/*62*/, TEXT("")/*63*/, TEXT("")/*64*/, TEXT("")/*65*/,

	//		Family Ziphiidae
	TEXT("Shepherd's beaked whale")/*66*/, TEXT("Baird's beaked whale")/*67*/, TEXT("Arnoux's beaked whale")/*68*/,
	TEXT("Longman's beaked whale")/*69*/, TEXT("Sowerby's beaked whale")/*70*/, TEXT("Blainville's beaked whale")/*71*/,
	TEXT("Gervais' beaked whale")/*72*/, TEXT("strap-toothed whale")/*73*/, TEXT("Hector's beaked whale")/*74*/,
	TEXT("Gray's beaked whale")/*75*/, TEXT("Stejneger's beaked whale")/*76*/, TEXT("Andrew's beaked whale")/*77*/,
	TEXT("True's beaked whale")/*78*/, TEXT("ginkgo-toothed beaked whale")/*79*/, TEXT("Hubb's beaked whale")/*80*/,
	TEXT("Pygmy beaked whale")/*81*/, TEXT("Cuvier's beaked whale")/*82*/, TEXT("northern bottlenose whale")/*83*/,
	TEXT("southern bottlenose whale")/*84*/, TEXT("")/*85*/, TEXT("")/*86*/, TEXT("")/*87*/, TEXT("")/*88*/, TEXT("")/*89*/,

	//		Family Physeteridae
	TEXT("sperm whale")/*90*/, TEXT("")/*91*/, TEXT("")/*92*/,

	//		Family Monodontidae
	TEXT("Irrawaddy dolphin, pesut")/*93*/, TEXT("white whale, beluga")/*94*/, TEXT("narwhal")/*95*/, TEXT("")/*96*/,


	//----------------------------//
	// Mysticetes (LF specialists)	
	//----------------------------//
	//		Generic
	TEXT("generic Mysticetes")/*97*/,

	//		Family Balaenidae
	TEXT("bowhead whale")/*98*/, TEXT("southern right whale")/**/, TEXT("northern right whale")/**/, TEXT("")/**/, TEXT("")/**/,

	//		Family Balaenopteridae		
	TEXT("minke whale")/**/, TEXT("sei whale")/**/, TEXT("Bryde's whale")/**/, TEXT("blue whale")/**/, TEXT("fin whale, finback")/**/, TEXT("humpback whale")/**/, TEXT("")/**/, TEXT("")/**/,

	//		Family Eschrichtiidae		
	TEXT("gray whale")/**/, TEXT("")/**/, TEXT("")/**/,

	//		Family Neobalaenidae		
	TEXT("pygmy right whale")/**/, TEXT("")/**/,

	//------------------//
	// Pinniped (Phocid)				
	//------------------//
	//		Generic
	TEXT("generic Phocid")/**/,

	//		Subfamily Phocinae			
	TEXT("Hooded seal")/**/, TEXT("Bearded seal")/**/, TEXT("Grey seal")/**/, TEXT("Caspian seal")/**/, TEXT("Ribbon seal")/**/, TEXT("Harp seal")/**/,	TEXT("Ringed seal")/**/,
	TEXT("Largha seal")/**/, TEXT("Baikal seal")/**/, TEXT("Harbor seal")/**/, TEXT("")/**/, TEXT("")/**/, TEXT("")/**/, TEXT("")/**/,

	//		Subfamily Monachinae			
	TEXT("Leopard seal")/**/, TEXT("Weddell seal")/**/, TEXT("Crabeater seal")/**/, TEXT("Northern elephant seal")/**/, TEXT("Southern elephant seal")/**/,
	TEXT("Mediterranean monk seal")/**/, TEXT("Hawaiian monk seal")/**/, TEXT("Ross seal")/**/, TEXT("")/**/, TEXT("")/**/,

	//------------------//
	// Pinniped (Otarrid)						
	//------------------//
	//		Generic
	TEXT("generic Otarrid")/**/,

	//		Subfamily Otariinae					
	TEXT("Steller sea lion")/**/, TEXT("Australian sea lion")/**/, TEXT("Southern sea lion, South American sea lion")/**/,
	TEXT("New Zealand sea lion, Hooker's sea lion")/**/, TEXT("California sea lion")/**/, TEXT("")/**/, TEXT("")/**/, TEXT("")/**/,

	//		Subfamily Arctocephalinae					
	TEXT("South American fur seal")/**/, TEXT("New Zealand fur seal")/**/, TEXT("Galápagos fur seal")/**/, TEXT("Antarctic fur seal")/**/,
	TEXT("Juan Fernández fur seal")/**/, TEXT("Australian fur seal, South African fur seal")/**/, TEXT("Guadalupe fur seal")/**/,
	TEXT("Subantarctic fur seal")/**/, TEXT("Northern fur seal")/**/, TEXT("")/**/, TEXT("")/**/, TEXT("")/**/,

	//-----------------------//
	// Special Considerations						
	//-----------------------//
	//		Generic
	TEXT("generic special consideration")/**/,

	//		Family Phocoenidae   					
	TEXT("harbor porpoise")/**/, TEXT("")/**/,

	//---------------------//
	// Other Marine Mammals						
	//---------------------//
	//		Generic
	TEXT("generic marine mammal")/**/,

	//		Pinnipeds					
	//			Family Odobenidae				
	TEXT("Walrus")/**/, TEXT("")/**/, TEXT("")/**/,

	//		Sirenians					
	//			Family Dugongidae				
	TEXT("Dugong")/**/, TEXT("")/**/, TEXT("")/**/,

	//			Family Trichechidae			
	TEXT("West Indian manatee")/**/, TEXT("Amazonian Manatee")/**/, TEXT("West African Manatee")/**/, TEXT("")/**/, TEXT("")/**/,

	//		Carnivores				
	//			Family Mustellidae			
	TEXT("Sea Otter")/**/, TEXT("Marine Otter")/**/, TEXT("")/**/, TEXT("")/**/,

	//			Family Ursidae			
	TEXT("Polar Bear")/**/, TEXT("")/**/,

	//------------//
	// Sea turtles					
	//------------//
	//		Generic
	TEXT("generic sea turtle")/**/,

	TEXT("Green turtle")/**/, TEXT("Loggerhead")/**/, TEXT("Kemp's ridley")/**/, TEXT("Olive ridley")/**/, TEXT("Hawksbill")/**/, TEXT("Leatherback")/**/, TEXT("OPENSEATURT1")/**/, TEXT("OPENSEATURT2")/**/, TEXT("OPENSEATURT3"),

	//---------------//
	// Sound Sources
	//---------------//
	TEXT("Sound Source"), TEXT("zzz"), TEXT("zz"), TEXT("z"), TEXT(""), TEXT(""), TEXT(""),

	//-----------//
	// Future Use
	//-----------//
	TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""),
	TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""),
	TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""),
	TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""), TEXT(""),

};

#endif //DATATYPES_SPECIESGROUP_H
