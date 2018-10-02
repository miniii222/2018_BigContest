from selenium import webdriver
import time
import pandas as pd
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import TimeoutException
from selenium.webdriver.common.by import By

dr = webdriver.Chrome('./chromedriver.exe') #chromedriver�������!!
dr.get('https://kobis.or.kr/kobis/business/mast/mvie/searchMovieList.do')

#������������
start = dr.find_element_by_css_selector('input[title="��������"]')
start.clear()
start.send_keys('20110101') #��������

end = dr.find_element_by_css_selector('input[title="��������"]')
end.clear()
end.send_keys('2011231')# ��������

dr.find_element_by_xpath('//*[@id="content"]/form[1]/fieldset/section/p/button[1]').click()
last_page = 158

a,b = (int(last_page/10), last_page%10)

def get_movie_info() :
    tbl = dr.find_element_by_css_selector('table[class = "boardList03"]')
    tbd = tbl.find_element_by_css_selector('tbody')
    trs = tbd.find_elements_by_css_selector('tr')
    
    for tr in trs :
        info=[]

        #���� ��������
        ul = dr.find_element_by_css_selector('ul[class = "tab_layer"]')
        lis = ul.find_elements_by_css_selector('li')
        lis[1].find_element_by_css_selector('a').click()

        
        #������ ���� ��������
        while True:
            try:
                wait = WebDriverWait(dr, 20)  #�ε��� �ʹ� ���� �ɸ��ٰ��ϸ� 20�κ� �� �ø��� �ɵ�?
                wait.until(EC.element_to_be_clickable((By.CSS_SELECTOR, 'li[class="btn"]')))
                #print("Page is ready!")
                tr = dr.find_element_by_css_selector('tr[class="withoutTop"]') #���⼭ error ���� ������ �ε��� �ȵż� �׷���
                tds = tr.find_elements_by_css_selector('td')
                for t in tds[1:6] :
                    info.append(t.text)
                break # it will break from the loop once the specific element will be present. 
            except TimeoutException:
                print("Loading took too much time!-Try again")

                    
        #��������
        try :
            tbl = dr.find_element_by_css_selector('table[class="board02 bma b02s"]')
            tbd = tbl.find_element_by_css_selector('tbody')
            trs = tbd.find_elements_by_css_selector('tr')
            tds = trs[1].find_elements_by_css_selector('td')
            info.append(tds[3].text)
        except :
            pass
            
        df = pd.DataFrame(info).T
        info=[] #�� ����Ʈ��
        df.to_csv("7777.csv",mode="a",header=False,index=False)

        #â �ݱ�
        dr.find_element_by_css_selector('a[class = "layer_close"]').click()

get_movie_info()

#ù ������
for i in [[1]]+list(range(3,13)) :    
    p = dr.find_element_by_css_selector('p[class = "pageList pList"]')
    aa = p.find_elements_by_css_selector('a')
    aa[i].click()

    tbl = dr.find_element_by_css_selector('table[class = "boardList03"]')
    tbd = tbl.find_element_by_css_selector('tbody')
    trs = tbd.find_elements_by_css_selector('tr')
    get_movie_info()

#�߰� ������
for j in range(a-1):
    for i in range(3,13) :
        p = dr.find_element_by_css_selector('p[class = "pageList pList"]')
        aa = p.find_elements_by_css_selector('a')
        aa[i].click()

        tbl = dr.find_element_by_css_selector('table[class = "boardList03"]')
        tbd = tbl.find_element_by_css_selector('tbody')
        trs = tbd.find_elements_by_css_selector('tr')
        get_movie_info()

#������ ������
for i in range(3,b+2) :
    p = dr.find_element_by_css_selector('p[class = "pageList pList"]')
    aa = p.find_elements_by_css_selector('a')
    aa[i].click()
    tbl = dr.find_element_by_css_selector('table[class = "boardList03"]')
    tbd = tbl.find_element_by_css_selector('tbody')
    trs = tbd.find_elements_by_css_selector('tr')
    get_movie_info()