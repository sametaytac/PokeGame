

social_network2= ( [(1,"a1","male",10,["a","b","c","d"]),(2,"a2","male",16,["a","b","e","f"]),(3,"a3","male",19,["a"]),(4,"a4","male",1,[]),(5,"a5","female",100,["d"]),(6,"a6","male",10,["a","b","c","d"]),(7,"a7","male",10,["b","c"]),(8,"a8","male",10,["a"]),(9,"a9","male",100,["b","d"]),(10,"a10","male",1,["c","d"]),(11,"a11","female",10,["d"]),(12,"a12","female",10,["a","b"]),(13,"a13","male",100,["c","d"]),(14,"a14","male",1,["c"]),(15,"a15","female",10,["a","b","c","d"]),(16,"a16","female",10,["a","b","c","d"]),(17,"a17","male",100,["a","b","d"]),(18,"a18","male",1,["m"]),(19,"a19","female",100,["a","b","l","z"]),(20,"a20","female",100,["b","c","l"])],[(1,5),(2,6),(3,7),(4,8),(5,9),(5,10),(6,9),(6,11),(7,10),(7,12),(8,11),(9,13),(9,14),(9,15),(10,13),(10,14),(10,15),(11,14),(11,15),(11,16),(12,13),(12,14),(12,15),(13,17),(13,18),(13,19),(13,20),(14,17),(14,18),(14,19),(14,20),(15,17),(15,18),(15,19),(15,20),(16,17),(16,18),(16,19),(16,20)] )









toptolist (a,b) =
			[a,b]



--pokemon pokemon tuple

pokbul pok (x:xs) =
			if (length xs)/=0 then
						if pok==head (toptolist x) then tail (toptolist x) ++ pokbul pok xs
						else if pok==last (toptolist x) then [head (toptolist x)] ++ pokbul pok xs
						else pokbul pok xs
			else if pok==head (toptolist x) then tail (toptolist x)
			else if pok==last (toptolist x) then [head (toptolist x)]
			else []

 -- pok list pok tuble

pokbul2 (x:xs) tuple =
			if (length xs)/=0 then pokbul x tuple ++ pokbul2 xs tuple
			else pokbul x tuple
			
-- tek kisi tuple,pok tuble			
yakala (a,b,c,d,e) f =  
						(a,b,c,d,(insertion_sort_lst (pokbul2 e f) e))
--social network pok tuple bos
--catch_pokemon (x:xs) tuple =
	--	if
--social network listesi,pok tuple list,bos
catch_pokemon1 (x:xs) a b = if (length xs)/=0 then catch_pokemon1 xs a (b ++ [yakala x a])
							else b ++ [yakala x a]
catch_pokemon a b =
					(catch_pokemon1 (fst a) b [],snd a)
		----------------------
--male kisi	

gender	a (x,_,b,_,_) =	
					if a==b then [x]
					else []

-- (above yas) kisi
yas (a,b) (x,_,_,y,_)  =
						if a=="above" then
											if y>b then [x]
											else []
						else if b>y then [x]
						else []

--kisiler male (above,10)					
get_statistics a b c =
						get_statistics1 (fst a) b c []
--list male above bos
get_statistics1 (x:xs) b c bos =
								if (length xs)/=0 then
														if yas c x == gender b x then get_statistics1 (xs) b c (bos++(yas c x))
														else get_statistics1 (xs) b c bos
								else if yas c x == gender b x then bos++(yas c x)
								else bos
				
--------------------------------
--arkadaslık list ,pair,uzaklık
nodebasla a (x,y) n =
					if dijkstra a x y <= n then True
				else False

esitmi a (x:xs) =
				 if (length xs)/=0 then
										if a==x then True
										else esitmi a xs
				 else if a==x then True
				 else False
					
--arkadaslık list,pair list uzaklık,orjinal arkadaslık list
get_fof_network1 a (x:xs) n b =
							if (length xs)/=0 then
													if (nodebasla a x n)==True && (esitmi x b)==False then get_fof_network1 a xs n  (insertion_sort_tuple [x] b)
													else get_fof_network1 a xs n b
							else if (nodebasla a x n)==True && (esitmi x b)==False then insertion_sort_tuple [x] b
							else b
--social network,pair,uzaklık
get_fof_network a b n =
						((fst a),(get_fof_network1 (snd a) b n (snd a)))

--------------------------
--tuple kisi,pokemon
tupleid (x,_,_,_,z) p =
							if esitmi p z ==True then [x]
							else []
--networkun bası,pokemon,bos
tupletop (x:xs) p bos = 
						if (length xs)/=0 then tupletop xs p (insertion_sort_lst bos (tupleid x p))
						else insertion_sort_lst bos (tupleid x p)
					
--networkun sonu,liste,uzaklik,kisi,bos
uzaklikbak a (x:xs) n id bos =
								if (length xs)/=0
													then if (dijkstra a x id)<=n && x/=id && (dijkstra a x id)/=1 then uzaklikbak a xs n id (insertion_sort_lst bos [x])
													else uzaklikbak a xs n id bos
								else if (dijkstra a x id)<=n && x/=id then insertion_sort_lst bos [x]
								else bos
--network,id,uzaklik,pokemon								
recomendation a id n pok =
							uzaklikbak (snd a) (tupletop (fst a) pok []) n id []

		
		
		
	---------------------------- MAIN FUNCTIONS ------------------------------------------

----distance between nodes
dijkstra graph baslangic bitis = dijkstra2 graph (tum_listeyi_olustur graph baslangic) baslangic bitis


----sorting lists
---------------------------------
insertion_sort_lst [] new_list = new_list
insertion_sort_lst (x:xs) new_list = insertion_sort_lst xs (insert_to_right_place_lst x new_list)

---sorting tuples
--------------------------------
insertion_sort_tuple [] new_list = new_list
insertion_sort_tuple (x:xs) new_list = insertion_sort_tuple xs (insert_to_right_place_tuple x new_list)





-----don't forget to copy also helpers because functions are also calling helpers, without them main functions won't work
-------------------------- HELPER FUNCTIONS -----------------------------------------

insert_to_right_place_tuple (a,b) [] = [(a,b)]
insert_to_right_place_tuple (a,b) ((x,y):xs) = if a < x then [(a,b)] ++ ((x,y):xs) else if a==x && b <= y then [(a,b)] ++ ((x,y):xs) else [(x,y)] ++ (insert_to_right_place_tuple (a,b) xs )

insert_to_right_place_lst a [] = [a]
insert_to_right_place_lst a (x:xs)= if a< x then [a] ++ (x:xs) else [x] ++ insert_to_right_place_lst a xs

dijkstra2 graph ((x,y):node_dist_pairs) a b= let choosen = fst (head (sort_tuples_to_dist ((x,y):node_dist_pairs) [] ) )
						 choosen_dist = (snd (head (sort_tuples_to_dist ((x,y):node_dist_pairs) [])))
						 cikarilacak_list= (cikarilacaklar_list_olustur graph (fst (head (sort_tuples_to_dist ((x,y):node_dist_pairs) [] ) )) []) 
					     in if choosen  == b then choosen_dist else  (dijkstra2 (graphtan_toplu_cikar graph choosen cikarilacak_list)  (modify_dist_pairs choosen_dist cikarilacak_list (tail (sort_tuples_to_dist ((x,y):node_dist_pairs) [] ))) a b  )


eleman_var_mi eleman []= False
eleman_var_mi eleman ((x,y):lst) = if x==eleman then True else eleman_var_mi eleman lst


ilk_eleman_yoksa_ekle eleman lst= if eleman_var_mi eleman lst then lst else lst++[(eleman,9999)]


tum_listeyi_olustur graph baslangic= (ikincileri_ekle graph (ilkleri_ekle graph [(baslangic,0)]))

ilkleri_ekle [] yeni_lst= yeni_lst
ilkleri_ekle ((x,y):lst) yeni_lst= ilkleri_ekle lst (ilk_eleman_yoksa_ekle x yeni_lst)

ikincileri_ekle [] yeni_lst= yeni_lst
ikincileri_ekle ((x,y):lst) yeni_lst= ikincileri_ekle lst (ilk_eleman_yoksa_ekle y yeni_lst)


cikarilacaklar_list_olustur [] choosen lst= lst
cikarilacaklar_list_olustur ((a,b):xs) choosen lst= if a==choosen then (cikarilacaklar_list_olustur xs choosen ([b]++lst)) else (if b==choosen then (cikarilacaklar_list_olustur xs choosen ([a]++lst)) else cikarilacaklar_list_olustur xs choosen lst)

graphtan_toplu_cikar graph choosen [] = graph
graphtan_toplu_cikar graph choosen (x:xs)= graphtan_toplu_cikar (graphtan_cikar graph (choosen,x)) choosen xs  


graphtan_cikar [] (first,second) = []
graphtan_cikar ((a,b):xs) (first,second) = if a==first && b==second then xs else (if a==second && b==first then xs else [(a,b)] ++ (graphtan_cikar xs (first,second)) ) 


sort_tuples_to_dist [] sorted_lst = sorted_lst
sort_tuples_to_dist (x:xs) sorted_lst = sort_tuples_to_dist xs (insert_to_place_tuple_dist x sorted_lst) 


insert_to_place_tuple_dist (a,b) [] = [(a,b)]
insert_to_place_tuple_dist (a,b) ((x,y):xs)= if b<y then [(a,b)] ++ ((x,y):xs) else [(x,y)] ++ (insert_to_place_tuple_dist (a,b) xs)

modifiye_edilecekler [] choosen modifiye_lst= modifiye_lst
modifiye_edilecekler ((a,b):xs) choosen modifiye_lst = if a==choosen then modifiye_edilecekler xs choosen [b]++modifiye_lst else (if b==choosen then   modifiye_edilecekler xs choosen [a]++modifiye_lst else modifiye_edilecekler xs choosen modifiye_lst )


modify_dist_pairs current_dist_choosen [] distance_list = distance_list
modify_dist_pairs current_dist_choosen (x:distance_koyulacaklar) distance_list = modify_dist_pairs current_dist_choosen distance_koyulacaklar (modify_dist current_dist_choosen x distance_list) 


modify_dist cur_dist element [] = []
modify_dist cur_dist element ((a,b):xs)= if a== element then (if cur_dist+1 < b then [(a,cur_dist+1)]  ++ xs else [(a,b)] ++xs) else [(a,b)] ++ (modify_dist cur_dist element xs) 


	
		
		
		
		
		
		
		
		
		
		
		
		
		
