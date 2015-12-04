package studententrend.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import studententrend.model.SbiCode;
import studententrend.model.SbiCode93;
import studententrend.model.dao.SbiCode93Repository;

@RestController
public class SbiCode93Controller {

	@Autowired
	SbiCode93Repository sbiCode93Repository;

	@RequestMapping(
			value = "/sbicodes93", 
			method = RequestMethod.GET,
			headers = "Accept=application/json", 
			produces = {"application/json"})
	@ResponseBody
	public List<SbiCode93> sbiCodeAll() {
		List<SbiCode93> sbiCodes93 = (List<SbiCode93>) sbiCode93Repository.findAll();
		
		for(SbiCode93 code : sbiCodes93){
			System.out.println(code.toString() + '\n');
		}
		
		return sbiCodes93;
	}
	
	@RequestMapping(
			value = "/sbicodes93/{sbi}", 
			method = RequestMethod.GET,
			headers = "Accept=application/json", 
			produces = {"application/json"})
	@ResponseBody
	public List<SbiCode93> sbiCodeBySbi(@PathVariable String sbi) {
		List<SbiCode93> sbiCodes93 = null;
		if(sbi.length() == 1) {
			sbiCodes93 = (List<SbiCode93>) sbiCode93Repository.findBySbiCode93(sbi);
		} else {
			sbi = sbi.replaceAll("_", " ");
			sbiCodes93 = (List<SbiCode93>) sbiCode93Repository.findBySbiNaam93(sbi);
		}		
		
		for(SbiCode93 code : sbiCodes93){
			System.out.println(code.toString() + '\n');
		}
		
		return sbiCodes93;
	}
}
