package studententrend.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import studententrend.model.OnderwijsSoort;
import studententrend.model.dao.OnderwijsSoortRepository;

@RestController
public class OnderwijsSoortController {
	@Autowired
	OnderwijsSoortRepository onderwijsSoortRepository;

	@RequestMapping(
			value = "/onderwijs", 
			method = RequestMethod.GET,
			headers = "Accept=application/json", 
			produces = {"application/json"})
	@ResponseBody
	public List<OnderwijsSoort> onderwijsSoortAll() {
		List<OnderwijsSoort> onderwijsSoorten = (List<OnderwijsSoort>) onderwijsSoortRepository.findAll();
		
		for(OnderwijsSoort onderwijsSoort : onderwijsSoorten){
			System.out.println(onderwijsSoort.toString() + '\n');
		}
		
		return onderwijsSoorten;
	}
	

	@RequestMapping(
			value = "/onderwijs/{ondCode}", 
			method = RequestMethod.GET,
			headers = "Accept=application/json", 
			produces = {"application/json"})
	@ResponseBody
	public List<OnderwijsSoort> onderwijsSoortByCodeOrNaam(@PathVariable("ondCode") String ondCode) {
		List<OnderwijsSoort> onderwijsSoorten = null;
		
		if(ondCode.length() <= 5) {
			onderwijsSoorten = (List<OnderwijsSoort>) onderwijsSoortRepository.findByOndCode(ondCode);
		} else {
			ondCode = ondCode.replaceAll("_", " ");
			onderwijsSoorten = (List<OnderwijsSoort>) onderwijsSoortRepository.findByOndNaam(ondCode);
		}
		
		for(OnderwijsSoort onderwijsSoort : onderwijsSoorten){
			System.out.println(onderwijsSoort.toString() + '\n');
		}
		
		return onderwijsSoorten;
	}
}
