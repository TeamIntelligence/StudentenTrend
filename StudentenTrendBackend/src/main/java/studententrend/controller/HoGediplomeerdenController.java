package studententrend.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import studententrend.model.HoGediplomeerde;
import studententrend.model.dao.HoGediplomeerdeRepository;

@RestController
public class HoGediplomeerdeController {
	@Autowired
	HoGediplomeerdeRepository hoGediplomeerdenRepository;

	@RequestMapping(
			value = "/gediplomeerden/ho", 
			method = RequestMethod.GET,
			headers = "Accept=application/json", 
			produces = {"application/json"})
	@ResponseBody
	public List<HoGediplomeerde> hoGediplomeerdenAll() {
		List<HoGediplomeerde> hoGediplomeerden = (List<HoGediplomeerde>) hoGediplomeerdenRepository.findAll();
		
		for(HoGediplomeerde hoGediplomeerde : hoGediplomeerden){
			System.out.println(hoGediplomeerde.toString() + '\n');
		}
		
		return hoGediplomeerden;
	}
	
}