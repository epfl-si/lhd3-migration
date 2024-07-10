insertBioOrgFilePath <- function(con, formHistory) {
  # get all bio_org to migrate
  result <- tbl(con, "bio_org") %>%
    # filter(id_bio_org %in% c(1,103,109,113,120, 19,23,31,39,44,52,6, 65,76,89,97,10, 105,11, 115,122,2, 25,33,4, 45,54,61,66,78,90,98,100,106,110,116,14,20,27,36,40,46,56,62,68,8, 91,99,101,107,111,117,15,21,3, 37,41,5, 57,63,7, 87,93,102,108,112,118,16,22,30,38,42,50,59,64,74,88,96)) %>%
    collect()
  
  listOfFiles <- c('100/Enterobacter cloacae.pdf','37/blank.pdf','45/Mycobacterium tuberculosis.pdf','42/Mycobacterium abscessus.pdf','56/Retroviral vectors_Ecotropic.pdf','103/Gutless Canine adenoviral vector.pdf','109/Mycobacterium bovis BCG RD1.pdf','46/Mycobacterium ulcerans.pdf','20/Clostridium septicum.pdf','30/Herpes virus amplicon vector.pdf','97/Clostridium sordellii.pdf','88/Prion-like proteins (aggregates).pdf','115/Vesicular stomatitis Indiana virus.pdf','66/Streptococcus pneumoniae.pdf','4/Adenoviral vector 5.pdf','122/Mycobacterium smegmatis.pdf','120/Human Cytomegalovirus.pdf','3/Acinetobacter baumannii.pdf','62/Serratia marcescens.pdf','102/Gutless Canine adenoviral vector.pdf','106/Influenza B virus.pdf','78/C. elegans GMC101.pdf','93/Candida glabrata.pdf','1/AAV vector.pdf','33/Human papillomavirus pseudovirus vector.pdf','64/Staphylococcus epidermidis.pdf','74/Environmental samples_Group 2.pdf','19/Clostridium difficile.pdf','21/Corynebacterium diphtheriae.pdf','31/Herpes simplex virus.pdf','54/RAW 264.7.pdf','118/Vibrio vulnificus.pdf','23/Echovirus.pdf','68/Vaccinia virus.pdf','108/Mycobacterium bovis.pdf','112/SARS-CoV-2 VSV pseudotypes vector.pdf','40/Listeria monocytogenes.pdf','27/Escherichia coli_Uropathogenic.pdf','7/Aeromonas hydrophila.pdf','96/Clostridium perfringens.pdf','36/Influenza A virus.pdf','111/SARS-CoV-2.pdf','105/Human respiratory syncytial virus.pdf','16/bersaglio.pdf','22/Ebola virus-like particles.pdf','76/Mycobacterium smegmatis.pdf','11/Aspergillus fumigatus.pdf','25/Enterococcus faecalis.pdf','110/Pseudorabies virus.pdf','5/Human Adenovirus type 37.pdf','65/Stenotrophomonas maltophilia.pdf','50/Human primary cells_ Group 2.pdf','117/Vibrio parahaemolyticus.pdf','52/Pseudomonas aeruginosa.pdf','90/Helicobacter pylori.pdf','113/Spiroplasma citris.pdf','2/AAV vector.pdf','107/Lymphocytic choriomeningitis virus.pdf','6/Aerococcus viridans.pdf','16.crap5/bersaglio.pdf','59/Salmonella typhimurium.pdf','89/Shigella flexneri.pdf','57/Retroviral vectors_Amphotropic.pdf','38/Lentiviral vectors_2nd generation.pdf','15/Canine adenoviral vector.pdf','99/Cryptosporidium parvum.pdf','10/Ascaris suum.pdf','98/Coxsackievirus.pdf','61/Sendai virus.pdf','41/Murid herpesvirus 1.pdf','44/Mycobacterium marinum.pdf','116/Vesicular Stomatitis Viral Vector.pdf','39/Lentiviral vectors_3rd generation.pdf','87/bersaglio.pdf','14/Candida albicans.pdf','8/Aeromonas salmonicida.pdf','63/Staphylococcus aureus.pdf','101/Fusarium oxysporum f. sp. Lycopersici.pdf','91/Bacteroides spp.pdf')
  listOfFilesModified <- lapply(listOfFiles, function(x) paste0('d_bio/', x))
  
  for (path in listOfFilesModified) {
    query <- sprintf(
      "UPDATE bio_org SET filePath = '%s' WHERE id_bio_org = %s",
      path,
      sub(".*?/([0-9]+).*", "\\1", path)
    )
    dbExecute(con, query)
  }
  # ---------------------------------------------------
}