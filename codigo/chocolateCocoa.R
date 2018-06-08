#se instala la libreria VIM para utilizar la funcion kNN de los vecinos mas
#cercanos
install.packages("VIM")

#se carga la libreria VIM
library(VIM)

#se carga la libreria car, para realizar las recodificaciones de las variables
#necesarios
library(car)

##3. LIMPIEZA DE LOS DATOS
#Lectura del archivo de chocolate sin ningun cambio
setwd("C:/")
cacao <- read.csv(file = "flavors_of_cacao.csv", header=TRUE)
#como se puede observar en el campo Tipo.de.frijol los elementos vacios
#se los representa por un caracter especial Â
head(cacao, 10)
head(cacao[,1:9])

#se le cambio al archivo original los caracteres especiales por una cadena
#vacia, para colocarle NA a todos los que estan a vacios, y R los reconozca 
#como vacios
cacao <- read.csv(file = "flavors_of_cacao1.csv", header=TRUE)
head(cacao, 10)

##3.1 ¿Los datos contienen ceros o elementos vacíos? 
#ceros y elementos vacios, se les coloca NA a todos los elementos vacios
for (i in 1:nrow(cacao)){
#print(cacao[i,8])
 for(j in 1:9){
   if (cacao[i,j] == ""){
      cacao[i,j] <- NA
   }
 }
}

#se comprueba el reemplazo del NA a los vacios
head(cacao, 10)

#se comprueba la linea 1545 que se encuentre con NA
print(cacao[1545,8])

#Para ver el tipo de dato asignado a cada campo
sapply(cacao, function(x) class(x))

#Para ver el numero de registros
nrow(cacao)

##3.1.1	Selección de los datos de interés
#Selección de los datos a analizar
#se elimina el campo 3 y 4 que no es de mucha utilidad para el análisis
cacao <- cacao[,-(3:4)]

#luego se elimina la columna 7 que en este caso en la primera carga era la 
#columna 9 de Origen del haba, se la elimina porque ya tenemos la columna 2
# que es el origen de la geo-region de origen especifica para la barra 
cacao <- cacao[,-7]
head(cacao, 10)

##3.1.2	¿Cómo gestionarías cada uno de estos casos?
#se comprueba los elementos vacios en este caso, porque ceros no existen
#se puede observar que existen 888 elementos vacios
sapply(cacao, function(x) sum(is.na(x))) 

#vamos a reemplazar esos elementos vacios por el vecino más cercano
#para trabajar con datos y no elementos vacios
#se reemplaza los vacios por el vecino mas cercano
cacao$Tipo.de.frijol <- kNN(cacao)$Tipo.de.frijol

#se comprueba los vacios que ya no existan
sapply(cacao, function(x) sum(is.na(x))) 

sapply(cacao, function(x) sum(is.nan(x))) 

#se revisa que se hayan reemplazado
head(cacao, 10)
nrow(cacao)
print(cacao)

########reemplazos en campo Empresa
#se reemplaza el nombre de la empresa Naive que en 3 registros se encuentra
#con caracteres especiales
cacao$Empresa <- recode(cacao$Empresa,'"Naï¿½ve"="Naive";
"Cacao de Origin"="Cacao de Origen";"Shattell"="Shattel";')

#se comprueba la linea 1166,1167,1168 que ya no tenga el caracter especial
print(cacao[1166,1])
print(cacao[1167,1])
print(cacao[1168,1])

#se comprueba que se haya reemplazado Cacao de Origin
print(cacao[296,1])

#se comprueba que se haya reemplazado Shattell
print(cacao[1456,1])

#se reemplaza tambien los ' a los campos Empresa, Geo-region, porque da 
#problemas pra recodificar a los registros
cacao$Empresa <- gsub("'","",cacao$Empresa)

#se comprueba las lineas 466,620, por ejemplo que ya no tenga el apostrofe
print(cacao[466,1])
print(cacao[620,1])

##########se reemplaza en la Geo-region
cacao$Geo.region <- gsub("'","",cacao$Geo.region)
#se comprueba el reemplazo
print(cacao[795,2])

#se reemplaza en la Geo-region el "
cacao$Geo.region <- gsub('"',"",cacao$Geo.region)
#se comprueba que se haya reemplazado
print(cacao[474,])
print(cacao[1608,])
print(cacao[282,])

#se reemplaza en la Geo-region el ;
cacao$Geo.region <- gsub(';',",",cacao$Geo.region)

#se comprueba que se haya reemplazado
print(cacao[292,])
print(cacao[978,])
print(cacao[293,])

#se reemplaza en la Porcentaje.de.cacao el %, para poder convertir
#a numerico los valores
cacao$Porcentaje.de.cacao <- gsub('%',"",cacao$Porcentaje.de.cacao)

#se comprueba que se haya reemplazado
print(cacao[,3])

#se reemplaza en la Geo-region el * y unos nombres de Equateur que corresponde a Ecuador y así con nombres de Geo-regiones
cacao$Geo.region <- recode(cacao$Geo.region,'"Concepcion*"="Concepcion";
"Capistrano*"="Capistrano";"Equateur"="Ecuador";"Ambolikapiky P."="Ambolikapiky";
"Ambolikapkly P."="Ambolikapiky";"Alto Beni, Palos Blanco"="Alto Beni, Palos Blancos";
"Chiapan"="Chiapas";"Brazilian"="Brazil";"Bolivian"="Bolivia";"Colombie"="Colombia";
"Colombian"="Colombia";"Dominican Republicm, rustic"="Dominican Republic, rustic";
"Fazenda Sempre Firme P., Bahia"="Fazenda Sempre Firme, Bahia";
"La Red, Guanconjeco"="La Red, Guaconejo";"Madagared"="Madagascar";
"Monte Alegre, Diego Badero"="Monte Alegre, D. Badero";"Nicaraqua"="Nicaragua";
"Trinidad-Tobago"="Trinidad & Tobago";"Venzuela"="Venezuela";
"Wild Bolivian"="Wild Bolivia";"Ba Lai"="Ba Ria";"Caraibe"="Carribean";
"Dominican"="Dominican Republic";"Elvesia"="Elvesia P.";')

#se comprueba que se haya reemplazado el *
print(cacao[1642,])
print(cacao[1639,])

#comprobar que se haya reemplazado el Equateur
print(cacao[376,])
print(cacao[1332,])

#comprobar que se haya reemplazado el Ambolikapiky P. y Ambolikapkly P.
print(cacao[265,2])
print(cacao[267,2])

#comprobar que se haya reemplazado el Alto Beni, Palos Blanco
print(cacao[172,2])

#comprobar que se haya reemplazado el Chiapan
print(cacao[1621,2])

#comprobar que se haya reemplazado el Brazilian
print(cacao[427,2])
print(cacao[428,2])

#comprobar que se haya reemplazado el Bolivian
print(cacao[533,2])

#comprobar que se haya reemplazado el Colombie
print(cacao[15,2])

#comprobar que se haya reemplazado el Colombian
#print(cacao[1423,2]) este tiene mas letras
print(cacao[494,2])

#comprobar que se haya reemplazado el Dominican Republicm, rustic
print(cacao[306,2])

#comprobar que se haya reemplazado el Fazenda Sempre Firme P., Bahia
print(cacao[1109,2])

#comprobar que se haya reemplazado La Red, Guanconjeco
print(cacao[902,2])

#comprobar que se haya reemplazado Madagared
print(cacao[571,2])

#comprobar que se haya reemplazado Monte Alegre, Diego Badero
print(cacao[1322,2])

#comprobar que se haya reemplazado Nicaraqua
print(cacao[1228,2])

#comprobar que se haya reemplazado Trinidad-Tobago
print(cacao[952,2])

#comprobar que se haya reemplazado Venzuela
print(cacao[924,2])

#comprobar que se haya reemplazado Wild Bolivian
print(cacao[395,2])

#comprobar que se haya reemplazado Ba Lai
print(cacao[626,2])

#comprobar que se haya reemplazado Caraibe
print(cacao[1698,2])

#comprobar que se haya reemplazado Dominican
print(cacao[1003,2])
#print(cacao[923,2])

#comprobar que se haya reemplazado Elvesia
print(cacao[539,2])
#print(cacao[364,2])

#se elimina el registro 246 porque contiene una region con el nombre:
#One Hundred que no existe
print(cacao[246,])
cacao <- cacao[-246,]

#se elimina el registro 779 porque contiene una region con el nombre:
#One Hundred que no existe
print(cacao[779,])
cacao <- cacao[-779,]

#se elimina el registro 1410 porque contiene una region con el nombre:
#100 percent que no existe
print(cacao[1410,])
cacao <- cacao[-1410,]

#quedaron 1792 registros
nrow(cacao)

######reemplazos en Tipo.de.frijol
cacao$Tipo.de.frijol <- recode(cacao$Tipo.de.frijol,'"Criollo, +"="Criollo";
"Forastero (Arriba) ASSS"="Forastero (Arriba) ASS";')

#comprobar que se haya reemplazado Criollo, +
print(cacao[767,6])

#comprobar que se haya reemplazado Forastero (Arriba) ASSS
print(cacao[1418,6])

#########
#se cambia las localizaciones que están mal, según el nombre
cacao$Localizacion <- recode(cacao$Localizacion,'"Eucador"="Ecuador";
"Niacragua"="Nicaragua";"Domincan Republic"="Dominican Republic";')

#se comprueba la de Ecuador que se encuentre correcto
print(cacao[1360,4])

#se comprueba la de Nicaragua que se encuentre correcto
print(cacao[1190,4])

#se comprueba la de Dominican Republic que se encuentre correcto
print(cacao[884,4])

###3.2 Identificación y tratamiento de valores extremos.
######DISCRETIZACION DE VARIABLES A NUMERICAS#####

#se discretiza la variable Empresa a valores numéricos
cacao$Empresa <- recode(cacao$Empresa,'"A. Morin"=1;
"Acalli"=2;
"Adi"=3;
"Aequare (Gianduja)"=4;
"Ah Cacao"=5;
"Akessons (Pralus)"=6;
"Alain Ducasse"=7;
"Alexandre"=8;
"Altus aka Cao Artisan"=9;
"Amano"=10;
"Amatller (Simon Coll)"=11;
"Amazona"=12;
"Ambrosia"=13;
"Amedei"=14;
"AMMA"=15;
"Anahata"=16;
"Animas"=17;
"Ara"=18;
"Arete"=19;
"Artisan du Chocolat"=20;
"Artisan du Chocolat (Casa Luker)"=21;
"Askinosie"=22;
"Bahen & Co."=23;
"Bakau"=24;
"Bar Au Chocolat"=25;
"Baravellis"=26;
"Batch"=27;
"Beau Cacao"=28;
"Beehive"=29;
"Belcolade"=30;
"Bellflower"=31;
"Belyzium"=32;
"Benoit Nihant"=33;
"Bernachon"=34;
"Beschle (Felchlin)"=35;
"Bisou"=36;
"Bittersweet Origins"=37;
"Black Mountain"=38;
"Black River (A. Morin)"=39;
"Blanxart"=40;
"Blue Bandana"=41;
"Bonnat"=42;
"Bouga Cacao (Tulicorp)"=43;
"Bowler Man"=44;
"Brasstown aka Its Chocolate"=45;
"Brazen"=46;
"Breeze Mill"=47;
"Bright"=48;
"Britarev"=49;
"Bronx Grrl Chocolate"=50;
"Burnt Fork Bend"=51;
"Cacao Arabuco"=52;
"Cacao Atlanta"=53;
"Cacao Barry"=54;
"Cacao de Origen"=55;
"Cacao Hunters"=56;
"Cacao Market"=57;
"Cacao Prieto"=58;
"Cacao Sampaka"=59;
"Cacao Store"=60;
"Cacaosuyo (Theobroma Inversiones)"=61;
"Cacaoyere (Ecuatoriana)"=62;
"Callebaut"=63;
"C-Amaro"=64;
"Cao"=65;
"Caoni (Tulicorp)"=66;
"Captain Pembleton"=67;
"Caribeans"=68;
"Carlotta Chocolat"=69;
"Castronovo"=70;
"Cello"=71;
"Cemoi"=72;
"Chaleur B"=73;
"Charm School"=74;
"Chchukululu (Tulicorp)"=75;
"Chequessett"=76;
"Chloe Chocolat"=77;
"Chocablog"=78;
"Choco Del Sol"=79;
"Choco Dong"=80;
"Chocolarder"=81;
"Chocolate"=82;
"Chocolate Alchemist-Philly"=83;
"Chocolate Con Amor"=84;
"Chocolate Conspiracy"=85;
"Chocolate Makers"=86;
"Chocolate Tree, The"=87;
"Chocolats Privilege"=88;
"ChocoReko"=89;
"Chocosol"=90;
"Chocovic"=91;
"Chocovivo"=92;
"Choklat"=93;
"Chokolat Elot (Girard)"=94;
"Choocsol"=95;
"Christopher Morel (Felchlin)"=96;
"Chuao Chocolatier"=97;
"Chuao Chocolatier (Pralus)"=98;
"Claudio Corallo"=99;
"Cloudforest"=100;
"Coleman & Davis"=101;
"Compania de Chocolate (Salgado)"=102;
"Condor"=103;
"Confluence"=104;
"Coppeneur"=105;
"Cote d Or (Kraft)"=106;
"Cravve"=107;
"Creo"=108;
"Daintree"=109;
"Dalloway"=110;
"Damson"=111;
"Dandelion"=112;
"Danta"=113;
"DAR"=114;
"Dark Forest"=115;
"Davis"=116;
"De Mendes"=117;
"De Villiers"=118;
"Dean and Deluca (Belcolade)"=119;
"Debauve & Gallais (Michel Cluizel)"=120;
"Desbarres"=121;
"DeVries"=122;
"Dick Taylor"=123;
"Doble & Bignall"=124;
"Dole (Guittard)"=125;
"Dolfin (Belcolade)"=126;
"Domori"=127;
"Dormouse"=128;
"Duffys"=129;
"Dulcinea"=130;
"Durand"=131;
"Durci"=132;
"East Van Roasters"=133;
"Eau de Rose"=134;
"Eclat (Felchlin)"=135;
"Edelmond"=136;
"El Ceibo"=137;
"El Rey"=138;
"Emerald Estate"=139;
"Emilys"=140;
"ENNA"=141;
"Enric Rovira (Claudio Corallo)"=142;
"Erithaj (A. Morin)"=143;
"Escazu"=144;
"Ethels Artisan (Mars)"=145;
"Ethereal"=146;
"Fearless (AMMA)"=147;
"Feitoria Cacao"=148;
"Felchlin"=149;
"Finca"=150;
"Forever Cacao"=151;
"Forteza (Cortes)"=152;
"Fossa"=153;
"Franceschi"=154;
"Frederic Blondeel"=155;
"French Broad"=156;
"Fresco"=157;
"Friis Holm"=158;
"Friis Holm (Bonnat)"=159;
"Fruition"=160;
"Garden Island"=161;
"Georgia Ramon"=162;
"Glennmade"=163;
"Goodnow Farms"=164;
"Grand Place"=165;
"Green & Blacks (ICAM)"=166;
"Green Bean to Bar"=167;
"Grenada Chocolate Co."=168;
"Guido Castagna"=169;
"Guittard"=170;
"Habitual"=171;
"Hachez"=172;
"Hacienda El Castillo"=173;
"Haigh"=174;
"Harper Macaw"=175;
"Heilemann"=176;
"Heirloom Cacao Preservation (Brasstown)"=177;
"Heirloom Cacao Preservation (Fruition)"=178;
"Heirloom Cacao Preservation (Guittard)"=179;
"Heirloom Cacao Preservation (Manoa)"=180;
"Heirloom Cacao Preservation (Millcreek)"=181;
"Heirloom Cacao Preservation (Mindo)"=182;
"Heirloom Cacao Preservation (Zokoko)"=183;
"hello cocoa"=184;
"hexx"=185;
"Hogarth"=186;
"Hoja Verde (Tulicorp)"=187;
"Holy Cacao"=188;
"Honest"=189;
"Hotel Chocolat"=190;
"Hotel Chocolat (Coppeneur)"=191;
"Hummingbird"=192;
"Idilio (Felchlin)"=193;
"Indah"=194;
"Indaphoria"=195;
"Indi"=196;
"iQ Chocolate"=197;
"Isidro"=198;
"Izard"=199;
"Jacque Torres"=200;
"Jordis"=201;
"Just Good Chocolate"=202;
"Kah Kow"=203;
"Kakao"=204;
"Kallari (Ecuatoriana)"=205;
"Kaoka (Cemoi)"=206;
"Kerchner"=207;
"Ki Xocolatl"=208;
"Kiskadee"=209;
"Kto"=210;
"Kul"=211;
"Kyya"=212;
"L.A. Burdick (Felchlin)"=213;
"La Chocolaterie Nanairo"=214;
"La Maison du Chocolat (Valrhona)"=215;
"La Oroquidea"=216;
"La Pepa de Oro"=217;
"Laia aka Chat-Noir"=218;
"Lajedo do Ouro"=219;
"Lake Champlain (Callebaut)"=220;
"LAmourette"=221;
"Letterpress"=222;
"Levy"=223;
"Lilla"=224;
"Lillie Belle"=225;
"Lindt & Sprungli"=226;
"Loiza"=227;
"Lonohana"=228;
"Love Bar"=229;
"Luker"=230;
"Machu Picchu Trading Co."=231;
"Madecasse (Cinagra)"=232;
"Madre"=233;
"Maglio"=234;
"Majani"=235;
"Malagasy (Chocolaterie Robert)"=236;
"Malagos"=237;
"Malie Kai (Guittard)"=238;
"Malmo"=239;
"Mana"=240;
"Manifesto Cacao"=241;
"Manoa"=242;
"Manufaktura Czekolady"=243;
"Map Chocolate"=244;
"Marana"=245;
"Marigolds Finest"=246;
"Marou"=247;
"Mars"=248;
"Marsatta"=249;
"Martin Mayer"=250;
"Mast Brothers"=251;
"Matale"=252;
"Maverick"=253;
"Mayacama"=254;
"Meadowlands"=255;
"Menakao (aka Cinagra)"=256;
"Mesocacao"=257;
"Metiisto"=258;
"Metropolitan"=259;
"Michel Cluizel"=260;
"Middlebury"=261;
"Millcreek Cacao Roasters"=262;
"Mindo"=263;
"Minimal"=264;
"Mission"=265;
"Mita"=266;
"Moho"=267;
"Molucca"=268;
"Momotombo"=269;
"Monarque"=270;
"Monsieur Truffe"=271;
"Montecristi"=272;
"Muchomas (Mesocacao)"=273;
"Mutari"=274;
"Nahua"=275;
"Naive"=276;
"Nanea"=277;
"Nathan Miller"=278;
"Neuhaus (Callebaut)"=279;
"Nibble"=280;
"Night Owl"=281;
"Noble Bean aka Jerjobo"=282;
"Noir d Ebine"=283;
"Nova Monda"=284;
"Nuance"=285;
"Nugali"=286;
"Oakland Chocolate Co."=287;
"Obolo"=288;
"Ocelot"=289;
"Ocho"=290;
"Ohiyo"=291;
"Oialla by Bojessen (Malmo)"=292;
"Olive and Sinclair"=293;
"Olivia"=294;
"Omanhene"=295;
"Omnom"=296;
"organicfair"=297;
"Original Beans (Felchlin)"=298;
"Original Hawaiin Chocolate Factory"=299;
"Orquidea"=300;
"Pacari"=301;
"Palette de Bine"=302;
"Pangea"=303;
"Park 75"=304;
"Parliament"=305;
"Pascha"=306;
"Patric"=307;
"Paul Young"=308;
"Peppalo"=309;
"Pierre Marcolini"=310;
"Pinellas"=311;
"Pitch Dark"=312;
"Pomm (aka Dead Dog)"=313;
"Potomac"=314;
"Pralus"=315;
"Pump Street Bakery"=316;
"Pura Delizia"=317;
"Q Chocolate"=318;
"Quetzalli (Wolter)"=319;
"Raaka"=320;
"Rain Republic"=321;
"Rancho San Jacinto"=322;
"Ranger"=323;
"Raoul Boulanger"=324;
"Raw Cocoa"=325;
"Republica del Cacao (aka Confecta)"=326;
"Ritual"=327;
"Roasting Masters"=328;
"Robert (aka Chocolaterie Robert)"=329;
"Rococo (Grenada Chocolate Co.)"=330;
"Rogue"=331;
"Rozsavolgyi"=332;
"S.A.I.D."=333;
"Sacred"=334;
"Salgado"=335;
"Santander (Compania Nacional)"=336;
"Santome"=337;
"Scharffen Berger"=338;
"Seaforth"=339;
"Shark Mountain"=340;
"Sharks"=341;
"Shattel"=342;
"Sibu"=343;
"Sibu Sura"=344;
"Silvio Bessone"=345;
"Sirene"=346;
"Sjolinds"=347;
"Smooth Chocolator, The"=348;
"Snake & Butterfly"=349;
"Sol Cacao"=350;
"Solkiki"=351;
"Solomons Gold"=352;
"Solstice"=353;
"Soma"=354;
"Somerville"=355;
"Soul"=356;
"Spagnvola"=357;
"Spencer"=358;
"Sprungli (Felchlin)"=359;
"SRSLY"=360;
"Starchild"=361;
"Stella (aka Bernrain)"=362;
"Stone Grindz"=363;
"StRita Supreme"=364;
"Sublime Origins"=365;
"Summerbird"=366;
"Suruca Chocolate"=367;
"Svenska Kakaobolaget"=368;
"Szanto Tibor"=369;
"Tabal"=370;
"Tablette (aka Vanillabeans)"=371;
"Tan Ban Skrati"=372;
"Taza"=373;
"TCHO"=374;
"Tejas"=375;
"Terroir"=376;
"The Barn"=377;
"Theo"=378;
"Theobroma"=379;
"Timo A. Meyer"=380;
"Toak (Ecuatoriana)"=381;
"Tobago Estate (Pralus)"=382;
"Tocoti"=383;
"Treehouse"=384;
"Tsara (Cinagra)"=385;
"twenty-four blackbirds"=386;
"Two Ravens"=387;
"Un Dimanche A Paris"=388;
"Undone"=389;
"Upchurch"=390;
"Urzi"=391;
"Valrhona"=392;
"Vanleer (Barry Callebaut)"=393;
"Vao Vao (Chocolaterie Robert)"=394;
"Vicuna"=395;
"Videri"=396;
"Vietcacao (A. Morin)"=397;
"Vintage Plantations"=398;
"Vintage Plantations (Tulicorp)"=399;
"Violet Sky"=400;
"Vivra"=401;
"Wellington Chocolate Factory"=402;
"Whittakers"=403;
"Wilkies Organic"=404;
"Willies Cacao"=405;
"Wm"=406;
"Woodblock"=407;
"Xocolat"=408;
"Xocolla"=409;
"Zaks"=410;
"Zart Pralinen"=411;
"Zokoko"=412;
"Zotter"=413;')

#se comprueba que se hayan transformado todos los valores a numéricos
#de la variable Empresa
print(cacao[,1])

#se discretiza la variable Geo.region a valores numéricos
cacao$Geo.region <- recode(cacao$Geo.region,'"heirloom, Arriba Nacional"=1;
"2009 Hapa Nibby"=2;
"A case of the Xerces Blues, triple roast"=3;
"Abinao"=4;
"ABOCFA Coop"=5;
"Abstract S. w/ Jamaica nibs,batch abs60323.0"=6;
"Acarigua, w/ nibs"=7;
"Acopagro"=8;
"Acul-du-Nord, 2015"=9;
"Africa"=10;
"Africa meets Latina"=11;
"AgroCriso Plantation"=12;
"Agua Fria, Sucre region"=13;
"Agua Grande"=14;
"Akata"=15;
"Akesson Estate"=16;
"Akesson P."=17;
"Akessons E., Sambirano V."=18;
"Akessons Estate"=19;
"Akessons Estate, Sambirano, 2013"=20;
"Akessons Estate, Sambirano, Ambanja"=21;
"Akessons, batch 4411"=22;
"Akosombo"=23;
"Almendra Blanca, batch 1004"=24;
"Alpaco"=25;
"Alta Verapaz, 2014"=26;
"Alto Beni"=27;
"Alto Beni, Covendo Region"=28;
"Alto Beni, Cru Savage"=29;
"Alto Beni, Palos Blancos"=30;
"Alto Beni, Upper Rio Beni, 2014"=31;
"Alto Beni, Upper Rio Beni, 2015"=32;
"Alto Beni, Wild Bolivian"=33;
"Alto Beni, Wild Harvest, Itenez R. 24hr c."=34;
"Alto Beni, Wild Harvest, Itenez R., 60hr c."=35;
"Alto Beni, Wild Harvest, Limited Ed."=36;
"Amazon Basin Blend"=37;
"Amazonas"=38;
"Amazonas Frucht"=39;
"Amazonas w/ nibs"=40;
"Amazonia"=41;
"Ambanja, batch 1 SRB"=42;
"Ambanja, Sambirano Valley"=43;
"Ambanja, Tsara Valley"=44;
"Ambolikapiky"=45;
"Amina"=46;
"Andoa, Grand Cru blend"=47;
"Ankasa"=48;
"Anselmo Paraiso Estate"=49;
"Antigua, Special Reserve"=50;
"Antilles (Trin/Gren/DR/Ven)"=51;
"Apurimac"=52;
"Apurimac, El Quinacho Co-op"=53;
"Aragua, Trincheras"=54;
"Araguani"=55;
"Aranama"=56;
"Arauca"=57;
"Arawak"=58;
"Arhuacos"=59;
"Arriba"=60;
"Asajaya E, NW Borneo, b. #132/4500"=61;
"Asante"=62;
"Asochivite, batch 1005"=63;
"Atsane"=64;
"Australia"=65;
"Autumn, Primary Harvest, 2012"=66;
"Ayacucho, El Guinacho"=67;
"Ba Ria"=68;
"Ba Ria Vung Tau Province"=69;
"Bachelors Hall E., St. Thomas Parish"=70;
"Bahia"=71;
"Bahia Black, batch bra50722.1"=72;
"Bahia Brazil, Fazenda Sao Pedro"=73;
"Bahia Superior"=74;
"Bahia, Agri-Forestal Plantation, 2010"=75;
"Bahia, Batch 148"=76;
"Bahia, batch a1213"=77;
"Bahia, Fazenda Camboa"=78;
"Bahia, Fazenda Venturosa"=79;
"Bahia, Floresta Azul,Good Friends Reserve#3"=80;
"Bahia, Scavina"=81;
"Baking"=82;
"Bali"=83;
"Bali (west), Sukrama Family, Melaya area"=84;
"Bali, Jembrana"=85;
"Bali, Singaraja"=86;
"Bali, Sukrama Bros. Farm, Melaya, 62hr C"=87;
"Balinese, Java"=88;
"Bambamarca, 2015"=89;
"Baracoa"=90;
"Baracoa, Cuba"=91;
"Barba, Xoco"=92;
"Barinas"=93;
"Barlovento, Venezuela"=94;
"Bayou Blend"=95;
"Belize"=96;
"Belize south"=97;
"Belize south, low fermentation"=98;
"Belize, 2013"=99;
"Belize, 2014 Harvest, Batch 9"=100;
"Belize, Batch 2"=101;
"Belize, med roast"=102;
"Bellavista Coop, #225, LR, MC, CG Exclusive"=103;
"Bellavista Gran Pajeten, San Martin"=104;
"Ben Tre"=105;
"Ben Tre, Dong Nai"=106;
"Ben Tre, Mekong Delta"=107;
"Ben Tre, Mekong Delta, MoCay"=108;
"Ben Tre, Surprise Valley"=109;
"Beniamo"=110;
"Birmanie"=111;
"Bittersweet"=112;
"black label"=113;
"Black Science Blend 1"=114;
"Blend"=115;
"Blend No. 1"=116;
"Blue Mountain"=117;
"Blue Mountain Region"=118;
"Bocas del Toro"=119;
"Bocas del Toro, Cocabo Co-op"=120;
"Bocas del Toro, Tierra Oscura"=121;
"Bolivar"=122;
"Bolivar, Arriba"=123;
"Bolivar, Guaranda"=124;
"Bolivia"=125;
"Bolivia, Bo-nib-ia, w/ nibs"=126;
"Bolivia, Wild Thing"=127;
"Boyaca, Aprocampa Coop, Pauna"=128;
"Brazil"=129;
"Brazil Blend"=130;
"Brazil Rio Doce"=131;
"Brazil, Batch 20316"=132;
"Brazil, Mitzi Blue"=133;
"Brooklyn Blend"=134;
"Bundibugyo"=135;
"Bundibugyo District"=136;
"Buto"=137;
"Cabosse"=138;
"Cacao Blanco"=139;
"Cacao Nacional W.F."=140;
"Cacao Nib Crunch"=141;
"Cacao Verapaz"=142;
"Cahabon"=143;
"Cahabon Region"=144;
"Camahogne"=145;
"Camino Verde"=146;
"Camino Verde P., 2012, Balao, Guayas"=147;
"Camino Verde P., Balao, 2015 harvest, batch8"=148;
"Camino Verde P., Balao, Guayas"=149;
"Camino Verde P., Balao, Guayas, Floral"=150;
"Camino Verde P., Balao, Guayas, Fruity"=151;
"Camino Verde P., Balao, Guayas, 2012"=152;
"Camino Verde P., Balao, Guayas, 2013"=153;
"Camino Verde P., Balao, Guayas, 2014"=154;
"Camino Verde, Balao, Guayas"=155;
"Camino Verde, Black S., batch cvu6030.0"=156;
"Camino Verde, Guayas"=157;
"Campesino w/ nibs"=158;
"Canoabo"=159;
"Canoabo, 2013"=160;
"Canoabo, Hacienda San Jose"=161;
"Capistrano"=162;
"Caracas, Venezuela and Ghana"=163;
"Caranero, Choc. Garage Exclusive"=164;
"Caraque"=165;
"Carenero"=166;
"Carenero S., Barlovento, Grand Cru"=167;
"Carenero Superior"=168;
"Carenero Superior, #203, MR, SC"=169;
"Carenero Superior, Apamate"=170;
"Carenero Superior, Bucare"=171;
"Carenero Superior, Concepcion"=172;
"Carenero Superior, Gran Saman"=173;
"Carenero Superior, Mijao"=174;
"Carenero Superior, Urrutia, Barlovento"=175;
"Carenero, Empyrean Sabor"=176;
"Carenero, Guapiles, Ocumare blend"=177;
"Caribe"=178;
"Carre Amer"=179;
"Carre Grand Noir"=180;
"Carribean"=181;
"Carribean-Raw"=182;
"Carupano, H. San Jose"=183;
"Castillo, Hispaniola, unroasted"=184;
"Catongo"=185;
"Cedeno, lot 271"=186;
"Cesar"=187;
"Ceylan"=188;
"Chanchamayo Province"=189;
"Chanchamayo, Pichanadi, 2012, 60hr c."=190;
"Chefs Blend"=191;
"Chiapas"=192;
"Chiapas, Lacandon Jungle"=193;
"Chiapas, Lacandon Jungle, Oaxacom Mtn"=194;
"Chiapas, Mokaya P."=195;
"Chiapas, Triple Cacao"=196;
"Chimelb, Lanquin, Alta Verapaz, b-GUA001"=197;
"Chocoan Rainforest, Teroro Escondido, ESM"=198;
"Chocolatey-beta"=199;
"Choobua, Kona"=200;
"Choroni"=201;
"Choroni, Finca Torres, 48hr c."=202;
"Chuao"=203;
"Chuao 100hr"=204;
"Chuao 2002 P."=205;
"Chuao 70hr"=206;
"Chuao, #217, DR, MC"=207;
"Chuao, #218, MR, MC"=208;
"Chuao, Aragua region"=209;
"Chuao, batch 3"=210;
"Chuao, Dark Roast"=211;
"Chuao, Hacienda San Jose"=212;
"Chuao, Light Roast"=213;
"Chuao, Mantuano blend"=214;
"Chuao, Med. Roast"=215;
"Chuao, Venezuela"=216;
"Chucuri"=217;
"Chulucanas, Batch 1"=218;
"Chulucanas, El Platanal"=219;
"Chuno"=220;
"Chuno, double turned, Xoco"=221;
"Chuno, San Jose de Bocay, Pantasma R.,B.S."=222;
"Chuno, triple turned, Xoco"=223;
"Chuno, Xoco"=224;
"CIAAB Coop"=225;
"Citrus-beta"=226;
"Classic"=227;
"Claudio Corallo w/ nibs"=228;
"Colombia"=229;
"Colombia, Batch 9"=230;
"Colombia, Casa Luker"=231;
"Colombian 2008"=232;
"Colombian Dark"=233;
"Colombian Semi Dark"=234;
"Colombian w/ nibs"=235;
"Complexite"=236;
"Conacado"=237;
"Conacado Coop"=238;
"Conacado, #212, LR, SC"=239;
"Conacado, #213, DR, -C"=240;
"Conacado, #223, MR, SC"=241;
"Conacado, #224, MR, MC"=242;
"Conacado, 2012, 120hr c."=243;
"Conacado, Manifesto"=244;
"Concepcion"=245;
"Congo"=246;
"Congo w/ nibs"=247;
"Congo, Gorilla bar"=248;
"Congo, Grand Cru"=249;
"Coopertiva Amazona"=250;
"Coopertiva Amazona w/ nibs"=251;
"Cooproagro"=252;
"Corazon del Ecuador, Calceta beans"=253;
"Cordoba"=254;
"Corona Arriba"=255;
"Cortes"=256;
"Costa Esmeraldas"=257;
"Costa Rica"=258;
"Costa Rica, Oscuro"=259;
"Cota Brus, Terciopelo, 2015"=260;
"Coto Brus"=261;
"Coto Brus, Heirloom, Batch 1"=262;
"Coto Brus, Terciopelo"=263;
"Coucher du Soleil"=264;
"Crayfish Bay aka Non Pariel Estate"=265;
"Crayfish Bay Estate, 2014"=266;
"Crazy 88"=267;
"Criollo Blend"=268;
"Criollo, Dominican Republic"=269;
"Criollo, Hawaii"=270;
"Crudo"=271;
"CSB Chama"=272;
"Cuana, 2008"=273;
"Cuana, 2013"=274;
"Cuba"=275;
"Cuba, Batch 59/100"=276;
"Cumbia"=277;
"Cumboto, farmer Jose Lugo"=278;
"Cusco"=279;
"Cusco, Cacao Cusco"=280;
"Cuyagua"=281;
"Cuyagua Village"=282;
"Cuyagua, 2013"=283;
"D.R. Congo, Cru Virunga"=284;
"Daintree Estates, N. Queensland"=285;
"Dak Lak, Batch 2451"=286;
"Dancing in Your Head, 5 bean blend"=287;
"Dark"=288;
"Dark 67"=289;
"Dark 75"=290;
"Dark, Stone Ground"=291;
"Davao"=292;
"Davao, Mt. Talamo foothills"=293;
"Diego 48hr/ W.F. blend prototype"=294;
"Diego 60hr/ W.F. blend prototype"=295;
"Diego/ original micro"=296;
"Djakarta, Java and Ghana"=297;
"Djual Island"=298;
"Dominican Republic"=299;
"Dominican Republic prototype"=300;
"Dominican Republic w/ nibs"=301;
"Dominican Republic, Love Bar"=302;
"Dominican Republic, Batch 3"=303;
"Dominican Republic, Batch 31616"=304;
"Dominican Republic, batch 7"=305;
"Dominican Republic, Batch D2"=306;
"Dominican Republic, Coop"=307;
"Dominican Republic, lot D82R"=308;
"Dominican Republic, rustic"=309;
"Dominican Republic-Organic"=310;
"Don Homero- Cerecita Valley"=311;
"Dong Nai"=312;
"Dos Rios"=313;
"Downtown London"=314;
"Dual Origins, Sambirano, Elvesia"=315;
"Duarte Province"=316;
"Duarte, Batch 360"=317;
"Duo- Gran Couva & Camino Verde"=318;
"DUO, batch 002"=319;
"Eastern Promises"=320;
"Ecuador"=321;
"Ecuador Puristique"=322;
"Ecuador, 2013"=323;
"Ecuador, Batch 1"=324;
"Ecuador, Batch 31516"=325;
"Ecuador, Bob Bar"=326;
"Ecuador, Choc. Garage Exclusive"=327;
"Ecuador, lot E432314L"=328;
"Ecuador, Midnight Dark"=329;
"Ecuador, Puristique"=330;
"Ecuador, raw"=331;
"Ecuador, Twilght Dark"=332;
"Ecuador, w/ nibs"=333;
"El Carmen, batch 1003"=334;
"El Ceibo Coop"=335;
"El Oro"=336;
"El Oro, Hacienda de Oro"=337;
"El Salvador"=338;
"Elvesia P."=339;
"Elvesia P., Batch 32"=340;
"Elvesia P., Black Science"=341;
"Elvesia, 2011"=342;
"Emerald Estate"=343;
"Epique, Blend No. 49"=344;
"Equator"=345;
"Esmeraldas"=346;
"Esmeraldas, Salazar Farm"=347;
"Espada"=348;
"Espiritu Santo, Smoke Monster"=349;
"Excellence (US Version)"=350;
"Extra Dark"=351;
"Fazenda Camboa"=352;
"Fazenda Camboa, Bahia"=353;
"Fazenda Leolinda"=354;
"Fazenda Sempre Firme, Bahia"=355;
"Finisterra"=356;
"Fleur de Cacao"=357;
"Fortissima"=358;
"French Laundry 20th Anniversary"=359;
"Fruity-beta"=360;
"Gabon"=361;
"Garaua"=362;
"Ghana"=363;
"Ghana prototype"=364;
"Ghana Puristique"=365;
"Ghana, #211, MR, MC"=366;
"Ghana, 2013"=367;
"Ghana, 2013, batch 129"=368;
"Ghana, Kumasi"=369;
"Ghana, Panama, Ecuador"=370;
"Ghana, prototype"=371;
"Goddess Blend"=372;
"Goodman Estate"=373;
"Gran Blanco"=374;
"Gran Couva"=375;
"Gran Couva 2005 P."=376;
"Grand Anse"=377;
"Grand Cru Blend No.1, 5 yr. Anniversary Ed"=378;
"Grand Cru Dominican Republic"=379;
"Grand Cru Ecuador"=380;
"Grand Cru Ghana"=381;
"Granella"=382;
"Grenada"=383;
"Grenada, Black Science"=384;
"Grenade"=385;
"Gru Grococo, St. Andrews"=386;
"Gruppo Salinas"=387;
"Guadalcanal"=388;
"Guadeloupe"=389;
"Guanaja"=390;
"Guaniamo"=391;
"Guaniamo, Amazonas"=392;
"Guantupi River"=393;
"Guapiles"=394;
"Guasare"=395;
"Guasare, La Sierra de Perija, batch gua001"=396;
"Guasare, Zulia Prov."=397;
"Guasare, Zulia Prov., 2015, batch 124"=398;
"Guatemala"=399;
"Guayas"=400;
"Guyave"=401;
"Hacienda la Trinidad"=402;
"Hacienda Las Trincheras"=403;
"Hacienda Victoria"=404;
"Haiti"=405;
"Haleiwa E, Oahu, 2014"=406;
"Haleiwa, Oahu, Lonohana E., Kanahiku"=407;
"Ham Luong"=408;
"Hamakua Coast, Kokoleka"=409;
"Hamakua, Hawaiian Crown, #176"=410;
"Haut Penja, w/ nibs"=411;
"Hawaii, Kona Estate Grown"=412;
"Hawaii, Kona Grand Cru E."=413;
"Hawaiian Crown, Kona Vanilla"=414;
"Hawaiian, Big Island"=415;
"Hilo"=416;
"Hilo, w/ added cocoa butter"=417;
"Hispaniola"=418;
"Hispaniola w/ nibs"=419;
"Hispaniola, 2008"=420;
"Hispaniola, 2013"=421;
"Hispaniola, batch 170104"=422;
"Honduras"=423;
"House Blend, Batch 2"=424;
"Houseblend"=425;
"Huallabamba, 2015"=426;
"Huila"=427;
"Huiwani Coop"=428;
"IL100, H. San Jose"=429;
"Ilblend"=430;
"India"=431;
"India (south)"=432;
"Indianer, Raw"=433;
"Indigena Amazonia, Grand Cru, Quizas"=434;
"Indio Rojo, Xoco"=435;
"Indonesia"=436;
"Island Growers, 120hr c."=437;
"Island Growers, 2012, 120hr c."=438;
"Island Growers, 96hr c."=439;
"Ivory Coast"=440;
"Ivory Coast, Batch 56/100"=441;
"Jamaica"=442;
"Jamaica a lancienne"=443;
"Jamaica, #204, DR, SC"=444;
"Jamaica, #205, DR, MC"=445;
"Jamaica, #206, DR, LC"=446;
"Jamaica, #209, DR, SC"=447;
"Jamaica, #210, DR, MC"=448;
"Jamaique"=449;
"Java"=450;
"Java, Grand Cru"=451;
"Java, Indonesian Black"=452;
"Java, Indonesie"=453;
"Java, Javablond"=454;
"Johe"=455;
"Johe, Xoco"=456;
"Juliana"=457;
"Jutiapa, lot 050916D"=458;
"Kafupbo, Petit Bourg, De Borgnes"=459;
"Kakao Kamili, Kilombero Valley"=460;
"Kakoa Kamili, Both Man & Bird & Beast"=461;
"Kaori"=462;
"Kauai"=463;
"KauaI, Alea Estate +world"=464;
"Kendari"=465;
"Kendem Lembu, Java"=466;
"Kerala State"=467;
"Kilombero Valley"=468;
"Kilombero, batch 41"=469;
"Kokoa Kamili"=470;
"Kokoa Kamili Coop"=471;
"Kokoa Kamili Coop, Kilombero"=472;
"Kokoa Kamili, batch 1 SRB"=473;
"Kolumbia"=474;
"Kongo, Highlands"=475;
"Kpime"=476;
"Kulili Estate"=477;
"Kulili P., 2013"=478;
"Kumasi Sambirano"=479;
"Kuruba"=480;
"la Amistad"=481;
"La Bahia, w/ cane juice"=482;
"La Bahia, w/ cane sugar"=483;
"La Dalia"=484;
"La Dalia, Matagalpa"=485;
"La Dalia, Matagalpa,cacao Bisesto,green label"=486;
"La Dorado, light roast"=487;
"La Masica, Batch 1, FHIA Research Center"=488;
"La Masica, Batch 7, FHIA"=489;
"La Masica, FHIA"=490;
"La Patriota, cacao Indio, purple label"=491;
"La Red"=492;
"La Red de Guanconejo, N. Highlands coop"=493;
"La Red, 2011"=494;
"La Red, Guaconejo"=495;
"La Red, Project Reserva, Guaconejo"=496;
"La Selva"=497;
"La Tronca, Matagalpa"=498;
"Lachua"=499;
"Lachua w/ cane sugar"=500;
"Lachua w/ maple sugar, batch 5"=501;
"Lachua, Qegchi families"=502;
"Lacri Blend"=503;
"Lago di Como, Blu"=504;
"Lam Dong"=505;
"Lam Dong, Batch 153"=506;
"LamasdelChanka, San Martin, Oro Verde coop"=507;
"Lanquin Estate"=508;
"Las Acacias E."=509;
"Las Islas"=510;
"Latino"=511;
"Le Chocolat Chaud"=512;
"Le Noir Amer"=513;
"Le Noir Extra Amer"=514;
"Lever du Soleil"=515;
"Libanio"=516;
"Liberia"=517;
"Liberia, #174"=518;
"Little Big Man"=519;
"Loma Los Pinos, Yacao region, D.R."=520;
"Loma Sotavento"=521;
"Loma Sotavento, 2013"=522;
"Los Ancones P."=523;
"Los Colorados, Santo Domingo, Equateur"=524;
"Los Llanos"=525;
"Los Rios"=526;
"Los Rios, H. Iara"=527;
"Los Rios, H. Iara, 2012"=528;
"Los Rios, H. Iara, 2012, 120hr c."=529;
"Los Rios, H. Iara, 96hr c."=530;
"Los Rios, Hacienda Limon, Heirloom"=531;
"Los Rios, Hacienda Limon, Orecao, 2014"=532;
"Los Rios, Hacienda Limon, Orecao, 2015"=533;
"Los Rios, Puerto Romero, Equateur"=534;
"Los Rios, Quevedo"=535;
"Los Rios, Quevedo, Arriba"=536;
"Los Rios, Rancho Grande 2004/2007"=537;
"Los Rios, Rancho Grande 2007"=538;
"Los Rios, Vinces"=539;
"Los Ujuxtes"=540;
"Lumas, 2015 Harvest, Batch 6, brown sugar"=541;
"Lumas, 2015 Harvest, Batch 7"=542;
"Mababa"=543;
"Machu Pichu"=544;
"Macondo"=545;
"Macuare, Miranda, Chloe formula"=546;
"Madagascar"=547;
"Madagascar w/ nibs"=548;
"Madagascar, 100% criollo"=549;
"Madagascar, Ambolikapiky P."=550;
"Madagascar, Batch 2"=551;
"Madagascar, Batch 59/100"=552;
"Madagascar, Batch 8"=553;
"Madagascar, Grand Cru"=554;
"Madagascar, lot M0403R"=555;
"Madagascar, Nosy Be Isle."=556;
"Madagascar, Sassy Bar"=557;
"Madagascar, w/ shell"=558;
"Mahali, Kasai"=559;
"Makwale Village, Kyela"=560;
"Maleku"=561;
"Malekula Island"=562;
"Malekula P., 2013"=563;
"Malgascio"=564;
"Manabi"=565;
"Mangaro P."=566;
"Manhattan"=567;
"Manickchand Estate"=568;
"Manjari"=569;
"Mantuano"=570;
"Mantuano, 2012"=571;
"Mara"=572;
"Marabel Farms"=573;
"Maracaibo"=574;
"Maracaibo Clasificado"=575;
"Maracaibo, El Rosario"=576;
"Maracaibo, El Vigia"=577;
"Maragda"=578;
"Maragnam"=579;
"Maralumi P."=580;
"Maranon"=581;
"Maranon Canyon"=582;
"Maranon Canyon, Fortunato No. 4"=583;
"Maranon, #227, LR, MC"=584;
"Maranon, #228, MR, SC"=585;
"Maranon, #229, MR, LC"=586;
"Maranon, #230, DR, LC"=587;
"Maranon, 2014"=588;
"Maranon, batch 2"=589;
"Maranon, Cajamarca"=590;
"Maranon, Fortunato No. 4"=591;
"Maranon, Good & Evil, w/ nibs"=592;
"Maranon, Joya Rara"=593;
"Maranura"=594;
"Marcial, single Cote, 2012"=595;
"Markham Valley"=596;
"Markham Valley, #219, LR, MC"=597;
"Markham Valley, #220, MR, MC"=598;
"Markham Valley, #221, DR, MC"=599;
"Markham Valley, #222, LR, 0C"=600;
"Matagalpa"=601;
"Matagalpa, Cacao Bisiesto"=602;
"Matasawalevu"=603;
"Matiguas"=604;
"Matina 1-6, prototype"=605;
"Maunawili, Oahu, Agri Research C., 2014"=606;
"Maunawili, Oahu, Agri Research C., 2015"=607;
"Maya Belize"=608;
"Maya Mountain"=609;
"Maya Mountain w/ nibs"=610;
"Maya Mountain, Toledo, Batch 29"=611;
"Maya Mtn"=612;
"Maya Mtn., Break Bar- Snark"=613;
"Maya Mtn, Batch 18, Heirloom"=614;
"Maya Mtn, Batch 454, Heirloom"=615;
"Maya Mtn, Moho R., Toledo D."=616;
"Maya Mtn, Moho R., Toledo D., 2015"=617;
"Medagla, Xoco"=618;
"Mekong Delta & Dong Nai"=619;
"Mekong Delta, early 2014 Harvest"=620;
"Mexico"=621;
"Mexico, Lot 28022016"=622;
"Mid Mountain, 2014"=623;
"Midnight"=624;
"Millot P., Ambanja"=625;
"Millot Plantation"=626;
"Mindo"=627;
"Misterio"=628;
"Moho River"=629;
"Moho River Valley"=630;
"Moho Valley"=631;
"Mombacho"=632;
"Momotombo"=633;
"Montanya"=634;
"Monte Alegre (Itacare), Brazil"=635;
"Monte Alegre, 3 diff. plantations"=636;
"Monte Alegre, D. Badaro, Raw, Organic"=637;
"Monte Alegre, D. Badero"=638;
"Montubia"=639;
"Mora Mora 2006"=640;
"Morobe"=641;
"Morogoro"=642;
"Morropon, Norandiono Coop, Piura"=643;
"Moxos"=644;
"Nacional"=645;
"Namau Village"=646;
"Namau Village, N. Taileva P., batch a2812"=647;
"Napa"=648;
"Nativo, Varzea"=649;
"Nature"=650;
"New Ireland"=651;
"Nibby"=652;
"Nicaliso, Xoco"=653;
"Nicalizo"=654;
"Nicaragua"=655;
"Nicaragua, American style"=656;
"Nicaragua, w/ inbs"=657;
"Nigeria"=658;
"Nine"=659;
"Nocturne"=660;
"Noir"=661;
"Noir Infini"=662;
"Non Pariel Estate"=663;
"Norandino, batch 161208"=664;
"Noula Coop"=665;
"Nourish"=666;
"Nube- prototype"=667;
"Nutty-beta"=668;
"Nyangbo"=669;
"Oahu"=670;
"Oahu, N. Shore, Waialua E., Kakoleka"=671;
"Oahu, N. Shore, Waialua Estate"=672;
"Oahu, N. Shore, Waialua Estate w/ nibs"=673;
"Oahu, Winward, #151, Maunawili district"=674;
"Ocumare"=675;
"Ocumare 61"=676;
"Ocumare 61, Puertomar"=677;
"Ocumare 67, Puertofino"=678;
"Ocumare 77"=679;
"Ocumare de la Costa"=680;
"Ocumare, Cumboto"=681;
"Ocumare, H. Cata, 48hr c."=682;
"Ocumare, H. Cata, w/ nibs"=683;
"Ocumare, Premier Cru, Quizas No. 2"=684;
"Ocumare, prototype"=685;
"Ocumare, Puerto Cabello"=686;
"Ocumare, Puerto Cabello, Venezuela"=687;
"Ocumare, Venezuela"=688;
"Oko Caribe"=689;
"Oko Caribe, batch 1 SRB"=690;
"Oko Caribe, DOR005"=691;
"Oko Caribe, Duarte P."=692;
"Oko Caribe, Duarte P., Collab w Chocosol"=693;
"Oko Caribe, Duarte Province, 2016 H."=694;
"Onyx"=695;
"Opaeula Estate, Oahu, Eleele"=696;
"Opaeula Estate, Oahu, Nene, CG Exclusive"=697;
"OPayo"=698;
"Opayo, Waslala"=699;
"Organic Dark"=700;
"Orinoco"=701;
"Orinoqua Region, Arauca"=702;
"Oro"=703;
"Oscuro"=704;
"Oscuro, Finca Chimelb"=705;
"Otucan, Grand Cru"=706;
"Pablino"=707;
"Palo Blanco w/ panela, Chulucanas"=708;
"Palo Blanco, Chulucanas"=709;
"Palos Blancos"=710;
"Palos Blancos + nibs"=711;
"Panama"=712;
"Panama, Raven"=713;
"Pangoa"=714;
"Pangoa, w/ nibs"=715;
"Papaua New Guinea"=716;
"Papouasie"=717;
"Papua"=718;
"Papua Kerafat"=719;
"Papua New Guinea"=720;
"Papua New Guinea, Batch 2"=721;
"Papua New Guinea, triple roast, batch 1"=722;
"Paramaribo, batch 20160043-01"=723;
"Patanemo"=724;
"Patanemo Vil., Carabobo State, Tisano family"=725;
"Patanemo, Epoch, Donaldo"=726;
"Pepiniere, single Cote"=727;
"Perfect Illusion"=728;
"Perla Negra"=729;
"Peru"=730;
"Peru + nibs"=731;
"Peru Brutus"=732;
"Peru- Ecuador"=733;
"Peru, Awagum bar"=734;
"Peru, Batch 1"=735;
"Peru, Batch 51/100"=736;
"Peru, Las Pampas P."=737;
"Peru, Madagascar"=738;
"Peruvian"=739;
"Peruvian Amazon"=740;
"Phantom"=741;
"Philly Blend, 5 plantations"=742;
"Piaroa, Amazonas, Batch 350"=743;
"Pichincha"=744;
"Pinchincha, Mindo, Coop Nueva Esper., 2015"=745;
"Pisa"=746;
"Piura"=747;
"Piura Blanco"=748;
"Piura Blanco, Norandino"=749;
"Piura Select, Cacao Blanc"=750;
"Piura, Apotequil, Porcelana 72hr c."=751;
"Piura, Blanco de Criollo"=752;
"Piura, Choc. Garage Exclusive"=753;
"Piura, Illanka, Quemazon"=754;
"Piura, Perou"=755;
"PNG, Devotion"=756;
"PNG, Nib Bar"=757;
"PNG, Revolution"=758;
"PNG, Voodoo"=759;
"Porcelana"=760;
"Porcelana, Apotequil"=761;
"Porcelana, Batch 5163"=762;
"Porcelana, Colombia, Amazonas"=763;
"Porcelana, Maracaibo, Palmira P. 2005"=764;
"Porcelana, Maracaibo, Palmira P. 2006"=765;
"Porcelana, Pariguan"=766;
"Porcelana, Pedegral"=767;
"Porcelana, Premier Cru, Quizas No. 1"=768;
"Porcelana, S. of Lake Maracaibo"=769;
"Porcelana, Sorotaima,Machiques,batch pcl001"=770;
"Porcelana, Tabasco"=771;
"Porcelana, Tabasco, Finca La Joya"=772;
"Porcelana, Tabasco, La Joya"=773;
"Porcelana, Tabasco, Limited Ed."=774;
"Porcelana, Tabasco, Marfil de Blanco"=775;
"Porcelana, Tabasco, Mexico"=776;
"Porcelana, Venezuela"=777;
"Porcelana, Zulia"=778;
"Presidio"=779;
"Principe"=780;
"Puerto Cabello"=781;
"Puerto Cabello, Mantuano"=782;
"Puerto Plata"=783;
"Puerto Quito, heirloom"=784;
"Puerto Rico"=785;
"Punta Galera, cacao Nacional, gold label"=786;
"Purple Haze"=787;
"Quetzalcoatl"=788;
"Quilla"=789;
"Quito"=790;
"Rainforest"=791;
"Raw"=792;
"Red Mayan, Xoco"=793;
"Red Vanilla"=794;
"Rico Rugoso, Xoco"=795;
"Rio Arriba"=796;
"Rio Caribe"=797;
"Rio Caribe Superior, Paria Penninsula"=798;
"Rio Caribe, Batch 7"=799;
"Rio Caribe, Cariaco"=800;
"Rio Caribe, Macuro"=801;
"Rio Caribe, Paria Penninsula"=802;
"Rio Caribe, Tepui Treasure"=803;
"Rio Dulce, Xoco"=804;
"Rio Eni"=805;
"Rio Peripa H."=806;
"Rio Tuma"=807;
"Rizek Cacao, Cibao Valley, Domin. Rep."=808;
"Rizek Cacao, Domin. Rep."=809;
"Roberto"=810;
"Robson Estate"=811;
"ROIG"=812;
"ROIG, 2014"=813;
"Roxborough, Tobago"=814;
"Rugoso"=815;
"Rugoso, Bad Fermentation"=816;
"Rugoso, Xoco"=817;
"Saidor Estate, Madang P."=818;
"Samana"=819;
"Samar, East Visayas region"=820;
"Sambirano"=821;
"Sambirano 2006"=822;
"Sambirano Valley"=823;
"Sambirano Valley, #214, LR, MC"=824;
"Sambirano Valley, #215, MR, MC"=825;
"Sambirano Valley, #216, MR, LC"=826;
"Sambirano Valley, 2012"=827;
"Sambirano Valley, batch 2477"=828;
"Sambirano Valley, Black Science, B-60307.0"=829;
"Sambirano Valley, Le 100%"=830;
"Sambirano, 2008"=831;
"Sambirano, 2009"=832;
"Sambirano, Akesson Estate"=833;
"Sambirano, Ambanja"=834;
"Sambirano, Ambanja, Madagascar"=835;
"Sambirano, Ampamakia 2005, Millot P."=836;
"Sambirano, batch 170102"=837;
"Sambirano, Menava P."=838;
"San Andres"=839;
"San Andres, American style"=840;
"San Andres, silk"=841;
"San Francisco de Macoris, Cibao region"=842;
"San Joaquin"=843;
"San Jose"=844;
"San Jose del Tambo"=845;
"San Juan"=846;
"San Juan de Cheni"=847;
"San Juan Estate"=848;
"San Juan Estate, Cherry Blossoms at Night"=849;
"San Juan Estate, Gran Couva"=850;
"San Martin"=851;
"San Martin, Amazonian Highlands"=852;
"San Martin, Batch 2"=853;
"San Martin, Bellavista Coop, #226, DR, MC"=854;
"Sang Yum Coop"=855;
"Sangre Grande P., Trinidad"=856;
"Santander"=857;
"Santo Domingo"=858;
"Sao Tome"=859;
"Sao Tome & Principe"=860;
"Sao Tome, Batch 151"=861;
"Satipo Pangoa region, 16hr conche"=862;
"Satipo Pangoa region, 20hr conche"=863;
"Satipo region, white label"=864;
"Selva"=865;
"Selva Maya"=866;
"Selvagem, Jari"=867;
"Semisweet"=868;
"Sensations Intense"=869;
"Serian E., NW Borneo, b. #134/3800"=870;
"Shake Shack"=871;
"Sharkey"=872;
"Sierra Nevada"=873;
"Sierra Nevada, Tutu Iku"=874;
"Signature Blend"=875;
"Silvestre, Batch 1, 2011"=876;
"Silvestre, Batch 7, 2013"=877;
"Silvestre, La Paz, Beni"=878;
"single estate"=879;
"Sisa 36hr/ W. F. blend prototype"=880;
"Sisas Secret/ original micro"=881;
"Solomon Island"=882;
"Solomon Island w/ nibs"=883;
"Somia Plantation"=884;
"Somia Plantation, 2012"=885;
"Somia Plantation, Akesson, 2012"=886;
"Somia Plantation, Sambirano, 70hr C"=887;
"Somia, 2013"=888;
"South America"=889;
"South America and Africa"=890;
"Special Maker Reserve"=891;
"Spring, Secondary Harvest, 2012"=892;
"Sri Lanka"=893;
"St. Lucia"=894;
"Star of Ecuador"=895;
"Star of Peru"=896;
"Suchitepequez E."=897;
"Supremo- SF"=898;
"Sur del Lago"=899;
"Sur del Lago Classificado"=900;
"Sur del Lago, Amiari Meridena, Zulia, 48hr c."=901;
"Sur del Lago, Amiari Meridena, Zulia, w/ nibs"=902;
"Sur del Lago, Merida"=903;
"Surfin"=904;
"Sylvestre, Oialla"=905;
"Tabasco"=906;
"Taino Secret"=907;
"Tainori"=908;
"Talamanca, Raul-Kekoldo community"=909;
"Tamarina"=910;
"Tan Phu Dong Island, Heart of Darkness"=911;
"Tan Phu Dong, Treasure Island"=912;
"Tangara"=913;
"Tanzania"=914;
"Tanzania, batch a1"=915;
"Tanzania, Party Bar"=916;
"Tapanti, light roast"=917;
"Tarakan"=918;
"Tawau, Oct. 2015 Harvest"=919;
"TCHOPro 60.5"=920;
"TCHOPro 68"=921;
"Tenende, Uwate"=922;
"Tenor"=923;
"Terreiro Velho P."=924;
"Terreiro Velho P. w/ sugar crystals"=925;
"Teyuna"=926;
"the lost city, gracias a dias, batch 362"=927;
"The Other One, Grand Cru"=928;
"Three Amigos(Chuao, Wild Bolivia, D.R.)"=929;
"Tien Giang"=930;
"Tien Giang, 2015, batch 10-2-16"=931;
"Tien Giang, batch 1 SRB"=932;
"Tien Giang, Black S., batch VIT60420.0"=933;
"Tien Giang, Gao Co-op"=934;
"Tingo Maria"=935;
"Tobago"=936;
"Tokiala"=937;
"Toledo District"=938;
"Toledo District, 2015 Harvest"=939;
"Toledo District, Maya"=940;
"Toledo District, w/ nibs"=941;
"Tome Acu"=942;
"Tome Acu E., Amazon Rainforest"=943;
"Toscano Black"=944;
"Tranquilidad, Batch 1"=945;
"Tranquilidad, Baures"=946;
"Tres Hombres"=947;
"Trinatario Treasure"=948;
"Trincheras"=949;
"Trinidad"=950;
"Trinidad & Tobago"=951;
"Trinidad, Heritage, Limited ed."=952;
"Trinitario"=953;
"Trinite"=954;
"Trintade, Sao Tome"=955;
"Tsaranta"=956;
"Tumaco"=957;
"Tumbes"=958;
"Tumbes Coop"=959;
"Tumbes, Zarumilla"=960;
"Tumbes, 2013 Harvest, Batch 8"=961;
"Tumbes, Batch 2"=962;
"Tumbes, Dear Mr. Finley, 2014"=963;
"Tumbes, Norandino"=964;
"Twilight"=965;
"Uba Budo"=966;
"Uganda"=967;
"Umoho R., Toledo District, San Felipe"=968;
"UNOCACE"=969;
"Upala"=970;
"Upala w/ nibs"=971;
"Upala, Batch 12"=972;
"Upala, Batch 18"=973;
"Uranga, Lot 22032016"=974;
"Vale do Juliana E., Atlantic Forest"=975;
"Vale do Juliana, w/ nibs"=976;
"Vanua Levu"=977;
"Vanua Levu, Ami-Ami-CA"=978;
"Vanua Levu, Toto-A"=979;
"Vanuatu"=980;
"Vanuatu, batch 2410"=981;
"Venezuela"=982;
"Venezuela, Barinos, Merida, Tachron"=983;
"Venezuela, batch 122"=984;
"Venezuela, Trinidad"=985;
"Vietnam"=986;
"Vietnam, Batch 50/100"=987;
"Vila Gracinda"=988;
"Villa Andina"=989;
"Vinces"=990;
"Virunga"=991;
"Waiahole, Oahu"=992;
"Wampusirpi Region"=993;
"Wampusirpi, batch 007"=994;
"Wasatch"=995;
"West Africa"=996;
"Wild Beni, Lower Rio Beni, Tranquilidad, 2014"=997;
"Wild Beni, Lower Rio Beni, Tranquilidad, 2015"=998;
"Wild Beniano, 2016, batch 128, Heirloom"=999;
"Wild Bolivia"=1000;
"Wild Bolivian, Batch 2"=1001;
"Wild Bolivian, Jungle Love"=1002;
"Wild Thing"=1003;
"Winak Coop, Napo"=1004;
"Winak, Sumaco"=1005;
"Xoconusco"=1006;
"Xoconusco, cacao Real"=1007;
"Xoconusco, Chiapas"=1008;
"Xocunusco, Chiapas, Pichucalco"=1009;
"Zorzal Reserva"=1010;
"Zorzal Reserva w/ Charles Kerchner"=1011;
"Zorzal Reserva, 2015 H., Kerchner"=1012;
')

#se comprueba que se hayan transformado todos los valores a numéricos
#de la variable Geo.region
print(cacao[,2])

#se discretiza la variable Localizacion a valores numéricos
cacao$Localizacion <- recode(cacao$Localizacion,'"Amsterdam"=1;"Argentina"=2;
"Australia"=3;"Austria"=4;"Belgium"=5;"Bolivia"=6;"Brazil"=7;"Canada"=8;"Chile"=9;
"Colombia"=10;"Costa Rica"=11;"Czech Republic"=12;"Denmark"=13;"Dominican Republic"=14;
"Ecuador"=15;"Fiji"=16;"Finland"=17;"France"=18;"Germany"=19;"Ghana"=20;"Grenada"=21;
"Guatemala"=22;"Honduras"=23;"Hungary"=24;"Iceland"=25;"India"=26;"Ireland"=27;
"Israel"=28;"Italy"=29;"Japan"=30;"Lithuania"=31;"Madagascar"=32;"Martinique"=33;
"Mexico"=34;"Netherlands"=35;"New Zealand"=36;"Nicaragua"=37;"Peru"=38;"Philippines"=39;
"Poland"=40;"Portugal"=41;"Puerto Rico"=42;"Russia"=43;"Sao Tome"=44;"Scotland"=45;
"Singapore"=46;"South Africa"=47;"South Korea"=48;"Spain"=49;"St. Lucia"=50;"Suriname"=51;
"Sweden"=52;"Switzerland"=53;"U.K."=54;"U.S.A."=55;"Venezuela"=56;"Vietnam"=57;"Wales"=58;',as.factor.result=FALSE)   


#se comprueba que se hayan transformado todos los valores a numéricos
#de la variable Localizacion
print(cacao[,4])

#se discretiza la variable Tipo.de.frijol a valores numéricos
cacao$Tipo.de.frijol <- recode(cacao$Tipo.de.frijol,'"Amazon"=1;
"Amazon mix"=2;
"Amazon, ICS"=3;
"Beniano"=4;
"Blend"=5;
"Blend-Forastero,Criollo"=6;
"CCN51"=7;
"Criollo"=8;
"Criollo (Amarru)"=9;
"Criollo (Ocumare 61)"=10;
"Criollo (Ocumare 67)"=11;
"Criollo (Ocumare 77)"=12;
"Criollo (Ocumare)"=13;
"Criollo (Porcelana)"=14;
"Criollo (Wild)"=15;
"Criollo, Forastero"=16;
"Criollo, Trinitario"=17;
"EET"=18;
"Forastero"=19;
"Forastero (Amelonado)"=20;
"Forastero (Arriba)"=21;
"Forastero (Arriba) ASS"=22;
"Forastero (Catongo)"=23;
"Forastero (Nacional)"=24;
"Forastero (Parazinho)"=25;
"Forastero(Arriba, CCN)"=26;
"Forastero, Trinitario"=27;
"Matina"=28;
"Nacional"=29;
"Nacional (Arriba)"=30;
"Trinitario"=31;
"Trinitario (85% Criollo)"=32;
"Trinitario (Amelonado)"=33;
"Trinitario (Scavina)"=34;
"Trinitario, Criollo"=35;
"Trinitario, Forastero"=36;
"Trinitario, Nacional"=37;
"Trinitario, TCGA"=38;', as.factor.result=FALSE)  

#se comprueba que se hayan transformado todos los valores a numéricos
#de la variable Tipo.de.frijol
print(cacao[,6])

#para convertir la variable Porcentaje.de.cacao de caracter a numeric
cacao$Porcentaje.de.cacao <- as.numeric(cacao$Porcentaje.de.cacao)

#Tranformación de variable Porcentaje.de.cacao, dividiendo para 100
cacao$Porcentaje.de.cacao <- cacao$Porcentaje.de.cacao/100

#comprueba el resultado de Porcentaje.de.cacao
print(cacao[,3])

#Para ver el tipo de dato asignado a cada campo
sapply(cacao, function(x) class(x))

#clustering para revisar valores extremos
set.seed(80) 
agrkm <- kmeans(cacao,centers=4)
print(agrkm)#se presenta todo con el names también
names(agrkm) #contenido del cluster

agrkm$cluster #asignación de observaciones a clusters
agrkm$totss #inercia total
agrkm$betweenss #inercia inter grupos, interesa que sea lo más alta posible
agrkm$withinss #inercia intra grupos
agrkm$tot.withinss #inercia intra grupos(total), interesa que sea lo menor posible

agrkm$centers #muestras los centros de los grupos es igual al aggregate

plot(cacao,col=agrkm$cluster)

#revisar los resultados de acuerdo a dos variables
#se puede observar que existen 3 registros con calificación 5 y estos pueden
#ser valores extremos, pero en este caso no se los elimina porque se los necesitará
#para realizar el análisis de los datos, ya que son los únicos valores con calificación alta
#y en el caso de los 4 registros que tienen calificación 1 tampoco se los elimina
#para su posterior análisis
plot(cacao$Geo.region,cacao$Calificacion,col=agrkm$cluster,xlab="Geo region",ylab="Calificacion")

#aquí no existen valores extremos
plot(cacao$Empresa,cacao$Geo.region,col=agrkm$cluster,xlab="Empresa",ylab="Geo.region")

#se puede diferenciar también que existen valores extremos cuando la calificación
#es 5 y cuando es 1, pero no se los elimina para su posterior análisis
plot(cacao$Localizacion,cacao$Calificacion,col=agrkm$cluster,xlab="Localizacion",ylab="Calificacion")

#se puede diferenciar tambien que existen valores extremos cuando la calificacion
#es 5 y cuando es 1, pero no se los elimina para su posterior analisis
plot(cacao$Porcentaje.de.cacao,cacao$Calificacion,col=agrkm$cluster,xlab="Porcentaje de cacao",ylab="Calificacion")

#se puede diferenciar también que existen valores extremos cuando la calificación
#es 5 y cuando es 1, pero no se los elimina para su posterior análisis
plot(cacao$Tipo.de.frijol,cacao$Calificacion,col=agrkm$cluster,xlab="Tipo de frijol",ylab="Calificacion")

#se puede diferenciar tambien que existen valores extremos cuando el porcentaje de cacao
#se encuentra en 40, 50 y 100%, pero no se los elimina para su posterior analisis
plot(cacao$Geo.region,cacao$Porcentaje.de.cacao,col=agrkm$cluster,xlab="Geo region",ylab="Porcentaje de cacao")

#con el boxplot tambien se puede diferenciar los valores extremos de las
#variables Calificacion y Porcentaje.de.cacao
boxplot.stats(cacao$Porcentaje.de.cacao)$out
boxplot.stats(cacao$Calificacion)$out

#exportación de los datos limpios en un nuevo archivo, el row.names=F es para
#que no grabe la primera columna que se genera con la secuencia de los datos
write.csv(cacao, "D:/flavors_of_cacao_clean.csv", row.names=F)

head(cacao, 10)

##############4. ANALISIS DE LOS DATOS#############
###4.1. Selección de los grupos de datos a analizar

#se realiza un histograma para determinar que conjuntos de datos son los de
#mayor interés del Porcentaje.de.cacao

hist(cacao$Porcentaje.de.cacao, breaks="Sturges", col="darkgray", main="Porcentaje de cacao",
xlab="Porcentaje de cacao",ylab="Frecuencia")

#Agrupacion por los porcentajes 0.70, 0.72 0.75 que son los que tienen 
#mayor cantidad de datos para analizar
cacao.porcentaje1 <- cacao[cacao$Porcentaje.de.cacao == 0.70,]
cacao.porcentaje2 <- cacao[cacao$Porcentaje.de.cacao == 0.72,]
cacao.porcentaje3 <- cacao[cacao$Porcentaje.de.cacao == 0.75,]

#se verifica la cantidad de datos
nrow(cacao.porcentaje1)
nrow(cacao.porcentaje2)
nrow(cacao.porcentaje3)

#total del conjunto de datos de acuerdo al porcentaje 1083 registros
nrow(cacao.porcentaje1)+nrow(cacao.porcentaje2)+nrow(cacao.porcentaje3)

#se realiza otro histograma para determinar que conjuntos de datos son los de
#mayor interés del Calificación de expertos
hist(cacao$Calificacion, breaks="Sturges", col="darkgray", main="Calificación de Expertos",
xlab="Calificacion",ylab="Frecuencia")

#Agrupación por calificación, entre los cuales los de mayor interés son:
#2.50, 2.75, 3, 3.25. 3.50, 3.75, como se puede verificar en el histograma

cacao.calificacion1 <- cacao[cacao$Calificacion == 2.50,]
cacao.calificacion2 <- cacao[cacao$Calificacion == 2.75,]
cacao.calificacion3 <- cacao[cacao$Calificacion == 3,]
cacao.calificacion4 <- cacao[cacao$Calificacion == 3.25,]
cacao.calificacion5 <- cacao[cacao$Calificacion == 3.50,]
cacao.calificacion6 <- cacao[cacao$Calificacion == 3.75,]

#total de registros del conjunto de datos de acuerdo a la Calificación de Expertos
#1632 registros
nrow(cacao.calificacion1)+nrow(cacao.calificacion2)+nrow(cacao.calificacion3)+nrow(cacao.calificacion4)+nrow(cacao.calificacion5)+nrow(cacao.calificacion6)

#subconjuntos
nrow(cacao.calificacion1)
nrow(cacao.calificacion2)
nrow(cacao.calificacion3)
nrow(cacao.calificacion4)
nrow(cacao.calificacion5)
nrow(cacao.calificacion6)


###4.2 Comprobación de la normalidad y homogeneidad de la varianza.
#Prueba de normalidad con el método de Anderson-Darling.
library(nortest)
alpha = 0.05
col.names = colnames(cacao)
print (col.names)

for (i in 1:ncol(cacao)) {
  if (i == 1) cat("Variables que no siguen una distribución normal:\n")
  if (is.integer(cacao[,i]) | is.numeric(cacao[,i])) {
    p_val = ad.test(cacao[,i])$p.value
    if (p_val < alpha) {
      cat(col.names[i])
      # Format output
      if (i < ncol(cacao) - 1) cat(", ")
      if (i %% 3 == 0) cat("\n")
    }
  }
}

#Homogeneidad de varianzas mediante el test de Fligner-Killeen:
#Se trata de un test no paramétrico que compara las varianzas basándose en la mediana
#De las variables obtenidas en el paso anterior, se toman 2 de ellas que son
#numéricas: Calificacion y Porcentaje.de.cacao
fligner.test(Calificacion ~ Porcentaje.de.cacao, data = cacao)


###4.3 Aplicación de pruebas estadísticas para comparar los grupos de datos.
#4.3.1 Covarianza y Correlación:
setwd("D:/")
cacao <- read.csv(file = "flavors_of_cacao_clean.csv", header=TRUE)
print (cacao)
cat("1: Empresa                \r\n");
cat("2: Geo.region             \r\n");
cat("3: Porcentaje.de.cacao    \r\n");
cat("4: Localizacion           \r\n");
cat("5: Calificacion           \r\n");
cat("6: Tipo.de.frijol         \r\n");
cat("                          \r\n");
num <- 0
cont <- 2
for (i in 1:ncol(cacao)){
  if (cont < ncol(cacao)+1){
    for (j in cont:ncol(cacao)){
      covarianza <- cov(cacao[[i]],cacao[[j]])
      correlacion <- cor(cacao[[i]],cacao[[j]])
      cat("Entre el campo: ",colnames(cacao)[i],"y el campo:",colnames(cacao)[j],"la covarianza es: ",covarianza,"\n\r");
      cat("Entre el campo: ",colnames(cacao)[i],"y el campo:",colnames(cacao)[j],"la correlación es: ",correlacion,"\n\r");
      
      plot(cacao[[i]],cacao[[j]],
           main = "Dispersión",
           ylab = paste("Campo",colnames(cacao)[j]),
           xlab = paste("Campo",colnames(cacao)[i]),
           col = "red")
      
      cat ("\n\r--------------\n\r");
      num <- num + 1
    }
  }
  cont <- cont + 1
}
cat("El número total de combinaciones es:", num)


#4.3.2 Prueba de contraste gráfico:
#Según se observa en la gráfica, en los campos Empresa y Geo.region es donde
#se encuentra la distribución de los datos en forma similar.
setwd("D:/")
cacao <- read.csv(file = "flavors_of_cacao_clean.csv", header=TRUE)

par(mfrow=c(2,2))
for(i in 1:ncol(cacao)) {
  if (is.numeric(cacao[,i])){
    qqnorm(cacao[,i],main = paste("Normal Q-Q Plot for ",colnames(cacao)[i]))
    qqline(cacao[,i],col="red")
    hist(cacao[,i], 
         main=paste("Histogram for ", colnames(cacao)[i]), 
         xlab=colnames(cacao)[i], freq = FALSE)
  }
}


#4.3.3 Regresión lineal
#La regresión lineal pertenece a los algoritmos supervisados, los cuales 
#tienen claro el objetivo que se persigue y generan un resultado concreto.
setwd("D:/")
cacao <- read.csv(file = "flavors_of_cacao_clean.csv", header=TRUE)

# Regresores cuantitativos
emp = cacao$Empresa
reg = cacao$Geo.region
por = cacao$Porcentaje.de.cacao
loc = cacao$Localizacion
tip = cacao$Tipo.de.frijol

# Variable a predecir
cal = cacao$Calificacion

# Generación de varios modelos
modelo1 <- lm(cal ~ emp + reg + tip + loc, data = cacao)
modelo2 <- lm(cal ~ reg + por + loc, data = cacao)
modelo3 <- lm(cal ~ reg + loc + tip, data = cacao)
modelo4 <- lm(cal ~ tip + loc + por, data = cacao)
modelo5 <- lm(cal ~ por + reg, data = cacao)

# Tabla con coeficientes de determinación de cada modelo
tabla.coeficientes <- matrix(c(1, summary(modelo1)$r.squared,
                               2, summary(modelo2)$r.squared,
                               3, summary(modelo3)$r.squared,
                               4, summary(modelo4)$r.squared,
                               5, summary(modelo5)$r.squared),
                             ncol = 2, byrow = TRUE)
colnames(tabla.coeficientes) <- c("Modelo", "R^2")
tabla.coeficientes

nuevodato <- data.frame(
  tip = 8,
  loc = 18,
  por = 0.9
)
# Predecir la calificación
predict(modelo4, nuevodato)



##5.Representación de los resultados a partir de tablas y gráficas
setwd("D:/")
cacao <- read.csv(file = "flavors_of_cacao_clean.csv", header=TRUE)
num <- 0
cont <- 2
for (i in 1:ncol(cacao)){
  if (cont < ncol(cacao)+1){
    for (j in cont:ncol(cacao)){
           correlacion <- cor(cacao[[i]],cacao[[j]])
           cat("Entre el campo: ",colnames(cacao)[i],"y el campo:",colnames(cacao)[j],"la correlación es: ",correlacion,"\n\r");
           plot(cacao[[i]],cacao[[j]],
           main = "Correlaciones entre campos",
           ylab = paste("Campo",colnames(cacao)[j]),
           xlab = paste("Campo",colnames(cacao)[i]),
           col = "red")
      cat ("\n\r--------------\n\r");
      num <- num + 1
    }
  }
  cont <- cont + 1
}
cat("El número total de combinaciones es:", num)



