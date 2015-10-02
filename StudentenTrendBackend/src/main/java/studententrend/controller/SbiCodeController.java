package studententrend.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import studententrend.model.SbiCode;
import studententrend.model.dao.SbiCodeRepository;

@RestController
public class SbiCodeController {

	@Autowired
	SbiCodeRepository sbiCodeRepository;

	@RequestMapping(
			value = "/sbicodes", 
			method = RequestMethod.GET,
			headers = "Accept=application/json", 
			produces = {"application/json"})
	@ResponseBody
	public List<SbiCode> sbiCodeAll() {
		List<SbiCode> sbiCodes = (List<SbiCode>) sbiCodeRepository.findAll();
		
		for(SbiCode code : sbiCodes){
			System.out.println(code.toString() + '\n');
		}
		
		return sbiCodes;
	}
	
	@RequestMapping(
			value = "/sbicodes/{sbi}", 
			method = RequestMethod.GET,
			headers = "Accept=application/json", 
			produces = {"application/json"})
	@ResponseBody
	public List<SbiCode> sbiCodeBySbi(@PathVariable String sbi) {
		List<SbiCode> sbiCodes = null;
		if(sbi.length() == 1) {
			sbiCodes = (List<SbiCode>) sbiCodeRepository.findBySbiCode(sbi);
		} else {
			sbi = sbi.replaceAll("_", " ");
			sbiCodes = (List<SbiCode>) sbiCodeRepository.findBySbiNaam(sbi);
		}		
		
		for(SbiCode code : sbiCodes){
			System.out.println(code.toString() + '\n');
		}
		
		return sbiCodes;
	}
}
