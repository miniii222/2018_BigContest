from selenium import webdriver
import time
import pandas as pd
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import TimeoutException
from selenium.webdriver.common.by import By

dr = webdriver.Chrome('./chromedriver.exe') #chromedriver경로지정!!
dr.get('https://kobis.or.kr/kobis/business/mast/mvie/searchMovieList.do')

#개봉일자지정
start = dr.find_element_by_css_selector('input[title="시작일자"]')
start.clear()
start.send_keys('20110101') #시작일자

end = dr.find_element_by_css_selector('input[title="종료일자"]')
end.clear()
end.send_keys('2011231')# 종료일자

dr.find_element_by_xpath('//*[@id="content"]/form[1]/fieldset/section/p/button[1]').click()
last_page = 158

a,b = (int(last_page/10), last_page%10)

def get_movie_info() :
    tbl = dr.find_element_by_css_selector('table[class = "boardList03"]')
    tbd = tbl.find_element_by_css_selector('tbody')
    trs = tbd.find_elements_by_css_selector('tr')
    
    for tr in trs :
        info=[]

        #다음 페이지로
        ul = dr.find_element_by_css_selector('ul[class = "tab_layer"]')
        lis = ul.find_elements_by_css_selector('li')
        lis[1].find_element_by_css_selector('a').click()

        
        #개봉전 정보 가져오기
        while True:
            try:
                wait = WebDriverWait(dr, 20)  #로딩이 너무 많이 걸린다고하면 20부분 더 늘리면 될듯?
                wait.until(EC.element_to_be_clickable((By.CSS_SELECTOR, 'li[class="btn"]')))
                #print("Page is ready!")
                tr = dr.find_element_by_css_selector('tr[class="withoutTop"]') #여기서 error 나면 페이지 로딩이 안돼서 그런거
                tds = tr.find_elements_by_css_selector('td')
                for t in tds[1:6] :
                    info.append(t.text)
                break # it will break from the loop once the specific element will be present. 
            except TimeoutException:
                print("Loading took too much time!-Try again")

                    
        #누적관객
        try :
            tbl = dr.find_element_by_css_selector('table[class="board02 bma b02s"]')
            tbd = tbl.find_element_by_css_selector('tbody')
            trs = tbd.find_elements_by_css_selector('tr')
            tds = trs[1].find_elements_by_css_selector('td')
            info.append(tds[3].text)
        except :
            pass
            
        df = pd.DataFrame(info).T
        info=[] #빈 리스트로
        df.to_csv("7777.csv",mode="a",header=False,index=False)

        #창 닫기
        dr.find_element_by_css_selector('a[class = "layer_close"]').click()

get_movie_info()

#첫 페이지
for i in [[1]]+list(range(3,13)) :    
    p = dr.find_element_by_css_selector('p[class = "pageList pList"]')
    aa = p.find_elements_by_css_selector('a')
    aa[i].click()

    tbl = dr.find_element_by_css_selector('table[class = "boardList03"]')
    tbd = tbl.find_element_by_css_selector('tbody')
    trs = tbd.find_elements_by_css_selector('tr')
    get_movie_info()

#중간 페이지
for j in range(a-1):
    for i in range(3,13) :
        p = dr.find_element_by_css_selector('p[class = "pageList pList"]')
        aa = p.find_elements_by_css_selector('a')
        aa[i].click()

        tbl = dr.find_element_by_css_selector('table[class = "boardList03"]')
        tbd = tbl.find_element_by_css_selector('tbody')
        trs = tbd.find_elements_by_css_selector('tr')
        get_movie_info()

#마지막 페이지
for i in range(3,b+2) :
    p = dr.find_element_by_css_selector('p[class = "pageList pList"]')
    aa = p.find_elements_by_css_selector('a')
    aa[i].click()
    tbl = dr.find_element_by_css_selector('table[class = "boardList03"]')
    tbd = tbl.find_element_by_css_selector('tbody')
    trs = tbd.find_elements_by_css_selector('tr')
    get_movie_info()