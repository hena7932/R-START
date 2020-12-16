#!/usr/bin/env python
# coding: utf-8

# In[ ]:





# # 6장. 문자열 조작
# 
# ## 00. 문자열 조작
# 
# **String Manipulation**  
# - 문자열을 변경하거나 분리하는 등 여러 과정  
# - 로우 레벨에서 조작하거나, C처럼 문자형이 따로 없는 언어에서는 조작이 까다로움
# - 단, 대부분 언어는 별도의 문자열 자료형과 문자열 조작을 위한 다양한 기능을 제공
# 
# **문자열 처리와 관련된 알고리즘이 쓰이는 분야**
# - 정보처리 분야: 여러 키워드로 웹 페이지를 탐색할 때 문자열 처리 애플리케이션을 이용
# - 통신 시스템 분야: 문자 메시지나 이메일을 보낼 때, 문자열을 어느 한 곳에서 다른 곳으로 보냄
# - 프로그래밍 시스템 분야: 프로그램 자체가 문자열로 구성, 컴파일러나 인터프리터 등은 문자열을 해석하고 처리하여 기계어로 변환하는 역할을 하는데 여기에 매우 정교한 문자열 처리 알고리즘이 쓰임
# 
# 
# ## 01. 유효한 팰린드롬
# 
# Q. 주어진 문자열이 팰린드롬인지 확인하라. 대소문자를 구분하지 않으며, 영문자와 숫자만을 대상으로 한다.  
# - 팰린드롬(Palindrome): 앞뒤가 똑같은 단어나 문장으로, 뒤집어도 같은 말이 되는 단어 또는 문장
# 
# 
# **- 입력**  
# 예제 1: "A man, a plan, a canal: Panama"  
# 예제 2: "race a car"  
# 
# 
# ### 풀이 1. 리스트로 변환
# 문자열을 직접 입력받아 펠린드롬 여부 판별하기

# In[1]:


class Solution:
    def isPalindrome(self, s: str) -> bool:
        strs = []
        for char in s:
            if char.isalnum(): #isalnum(): 영문자, 숫자 여부 판별하여 False, True 변환
                strs.append(char.lower()) # 모든 문자 소문자 변환하여 str에 입력
                print('문자 처리: ', strs)
                
        # 팰린드롬 여부 판별
        while len(strs) > 1: #strs의 길이가 1 이상이면 반복
            
            #pop(0): 맨 앞의 값, pop(): 맨 뒤의 값을 가져옴
            if strs.pop(0) != strs.pop():
                return False
        return True
    
if __name__ == '__main__':
  print('실행합니다: main')
#현재 스크립트 파일이 프로그램 시작점이 맞는지 판단
#스크립트 파일이 메인 프로그램으로 사용될 때와 모듈로 사용될 때를 구분하기 위함

  s = Solution()
  print(s.isPalindrome("A man, a plan, a canal: Panama"))
  print(s.isPalindrome("race a car"))


# ### 풀이 2. 데크 자료형을 이용한 최적화
# 데크(deque)를 명시적으로 선언하여 풀이 속도 개선하기  
# - deque: double-ended queue; 양방향에서 데이터를 처리할 수 있는 queue형 자료구조

# In[2]:


import collections
from typing import Deque

class Solution:
    def isPalindrome(self, s: str) -> bool:

        # 자료형 데크로 선언
        strs: Deque = collections.deque() #데크 생성
        print('\n데크 생성: ', strs)
        
        for char in s:
            if char.isalnum():
                strs.append(char.lower())
                print('문자 처리: ', strs)
                
        while len(strs) > 1:
            if strs.popleft() != strs.pop(): #데크의 popleft()는 O(1), 리스트의 pop(0)이 O(n)
                return False

        return True
    
if __name__ == '__main__':
  s = Solution()
  print(s.isPalindrome("A man, a plan, a canal: Panama"))
  print(s.isPalindrome("race a car"))


# ### 풀이 3. 슬라이싱 사용
# 정규식으로 불필요한 문자를 필터링하고 문자열 조작을 위해 슬라이싱 사용  
# re.sub('정규표현식', 대상 문자열, 치환 문자)  
# - 정규표현식: 검색 패턴 지정
# - 대상 문자열: 검색 대상이 되는 문자열
# - 치환 문자: 변경하고 싶은 문자

# In[3]:


import re #정규표현식 불러오기

class Solution:
    def isPalindrome(self, s: str) -> bool:
        s = s.lower()
        # 정규식으로 불필요한 문자 필터링: re.sub(''정규표현식', 대상 문자열, 치환 문자)
        s = re.sub('[^a-z0-9]', '', s) #s 중, 알파벳과 숫자가 아닌 것을 ''로 바꿔라
        print('\n문자 처리: ', s)

        return s == s[::-1]  # 슬라이싱 [::-1]: 배열 뒤집기

if __name__ == '__main__':
  s = Solution()
  print(s.isPalindrome("A man, a plan, a canal: Panama"))
  print(s.isPalindrome("race a car"))


# ## 02. 문자열 뒤집기
# Q. 문자열을 뒤집는 함수를 작성하라. 입력값은 문자 배열이며, 리턴 없이 리스트 내부를 직접 조작하라.  
# 
# **-입력**  
# 예제 1: ["h", "e", "l", "l", "o"]  
# 예제 2: ["H", "a", "n", "n", "a", "H"]
# 
# ### 풀이 1. 투 포인터를 이용한 스왑
# - 투 포인터(Two Pointer): 2개의 포인터를 이용해 범위를 조정해가며 풀이하는 방식  
# 
# 문제에 '리턴 없이 리스트 내부를 직접 조작하라'는 제약이 있으므로 s 내부를 스왑하는 형태로 풀이할 수 있음

# In[4]:


from typing import List

class Solution:
    def reverseString(self, s: List[str]) -> None:
        left, right = 0, len(s) - 1
        while left < right:
            s[left], s[right] = s[right], s[left]
            left += 1
            right -= 1
        return s
    
if __name__ == '__main__':
  s = Solution()
  print(s.reverseString(["h", "e", "l", "l", "o"]))
  print(s.reverseString(["H", "a", "n", "n", "a", "H"]))


# ### 풀이 2. 파이썬다운 방식
# 파이썬의 기본 기능을 이용하면 한 줄 코드로 불 수 있음
# 
# - reverse() 함수: 리스트에만 제공되어, 문자열의 경우에는 문자열 슬라이싱으로 풀이

# In[5]:


from typing import List

class Solution:
    def reverseString(self, s: List[str]) -> None:
        s.reverse() #리버스는 값을 반환해주지 않고 단순히 해당 list를 뒤섞음, None 반환
        return s #None 반환 대신 값 반환을 위해 사용
        
if __name__ == '__main__':
  s = Solution()
  print(s.reverseString(["h", "e", "l", "l", "o"]))
  print(s.reverseString(["H", "a", "n", "n", "a", "H"]))


# ## 03. 로그 파일 재정렬
# Q. 로그를 재정렬하라. 기준은 아래와 같다.  
#   1. 로그의 가장 앞 부분은 식별자다.
#   2. 문자로 구성된 로그가 숫자 로그보다 앞에 온다.
#   3. 식별자는 순서에 영향을 끼치지 않지만, 문자가 동일한 경우 식별자 순으로 한다.
#   4. 숫자 로그는 입력 순서대로 한다.
# 
#  
# **- 입력**  
# logs = ["dig1 8 1 5 1","let1 art can","dig2 3 6","let2 own kit dig","let3 art zero"]
# 
# 
# ### 풀이 1. 람다와 + 연산자 이용
# 요구 조건을 얼마나 깔끔하게 처리할 수 있는지를 묻는 문제로, 실무에서도 자주 쓰이는 로직  
# 
# - 조건 2, 문자로 구성된 로그가 숫자 로그 전에 오며, 숫자 로그는 입력 순서대로 둠
#   - 문자와 숫자 구분, 숫자는 그대로 이어 붙임
#   - isdigit()을 이용하여 숫자 여부를 판별해 구분해야 함

# In[6]:


from typing import List

class Solution:
    def reorderLogFiles(self, logs: List[str]) -> List[str]:
        letters, digits = [], [] #문자, 숫자 구분
        
        for log in logs:
            if log.split()[1].isdigit(): #숫자로 변환 가능한지 확인
                digits.append(log)       #변환되면 digits에 추가
            else:                        #변환되지 않으면 letters에 추가
                letters.append(log)

        # 두 개의 키를 람다 표현식으로 정렬
        # 식별자를 제외한 문자열 [1:]을 키로 정렬하며 동일한 경우 후순위로 식별자 [0]을 지정해 정렬되도록 람자 표현식으로 정렬
        letters.sort(key=lambda x: (x.split()[1:], x.split()[0]))
        
        # 문자 + 숫자 순서로 이어 붙이고 return
        return letters + digits

if __name__ == '__main__':
  s = Solution()
  print(s.reorderLogFiles(["dig1 8 1 5 1","let1 art can","dig2 3 6","let2 own kit dig","let3 art zero"]))


# ## 04. 가장 흔한 단어
# Q. 금지된 단어를 제외한 가장 흔하게 등장하는 단어를 출력하라. 대소문자 구분을 하지 않으며, 구두점(마침표, 쉼표 등) 또한 무시한다.  
# 
# **- 입력**  
# paragraph = "Bob hit a ball, the hit BALL flew far after it was hit."  
# banned = ["hit"]
# 
# 
# ### 풀이 1. 리스트 컴프리헨션, Counter 객체 사용
# 입력값에 대소문자가 섞여 있고 쉼표 등의 구두점이 존재하므로 전처리 작업이 필요(Data Cleansing)  
# 
# - 정규식 사용 코드
#   - \w: 단어 문자(Word Character)
#   = ^: not

# In[7]:


import collections
import re
from typing import List

class Solution:
    def mostCommonWord(self, paragraph: str, banned: List[str]) -> str:
        words = [word for word in re.sub(r'[^\w]', ' ', paragraph)
            .lower().split() #소문자와 ' ' 기준으로 쪼개기
                 if word not in banned] #banned를 제외한 단어 저장(예제에서는 hit)
        print('단어 처리: ', words)
            
        counts = collections.Counter(words)
        
        # 가장 흔하게 등장하는 단어의 첫 번째 인덱스 리턴
        # (1)은 n을 의미하며, 2차원이므로 [0][0]을 이용
        return counts.most_common(1)[0][0]

if __name__ == '__main__':
  s = Solution()
  print('\n',s.mostCommonWord(paragraph = "Bob hit a ball, the hit BALL flew far after it was hit.",
                         banned = ["hit"]))


# ## 05. 그룹 애너그램
# Q. 문자열 배열을 받아 애너그램 단위로 그룹핑하라.
# 
# **-입력**  
# ["eat", "tea", "tan", "ate", "nat", "bat"]
# 
# 
# ### 풀이 1. 정렬하여 딕셔너리에 추가
# 애너그램 관계인 단어들을 정렬하면 서로 같은 값을 갖기 때문에 정렬하여 비교하는 것이 애너그램을 판단하는 가장 간단한 방법  
# 파이썬의 딕셔너리는 키/값 해시 테이블 자료형  
# 
# - 사용 함수
#   - sorted(): 문자열도 정리하며 결과를 리스트 형태로 리턴
#   - join(): sorted된 결과를 키로 사용하기 위해 합쳐서 값을 키로 하는 딕셔너리로 구성  
#   - append(): 리스트에 요소 추가

# In[8]:


import collections
from typing import List


class Solution:
    def groupAnagrams(self, strs: List[str]) -> List[List[str]]:
       
        #존재하지 않는 키를 삽입하려 할 경우, keyError 발생 
        # default를 list로 설정하여 .append 기능 사용하기
        # value 값은 list 디폴트
        anagrams = collections.defaultdict(list)
        print('anagrams 확인: ', anagrams)
        
        for word in strs:
            # 정렬하여 딕셔너리에 추가
            # sorted()는 문자열도 정렬하며 결과를 리스트 형태로 리턴함
            # 리턴된 리스트 형태를 키로 사용하기 위해 join()으로 합치고 이를 키로 하는 딕셔너리 구성
            # list는 key 값을 쓰지 못하기 때문에 join() 함수는 리스트를 문자열로 바꾸게 됨
            # ' ': 문자 사이에 공백 추가
            anagrams[''.join(sorted(word))].append(word)
            
        return list(anagrams.values())

if __name__ == '__main__':
  s = Solution()
  print('\n',s.groupAnagrams(["eat", "tea", "tan", "ate", "nat", "bat"]))


# ### 추가. 여러 가지 정렬 방법
# 파이썬에서 시작된 고성능 정렬 알고리즘, 팀소트(Timsort) 살펴보기  
# 
# #### 1. sorted() 함수를 이용한 파이썬 리스트 정렬

# In[9]:


# 숫자 정렬
a = [2,5,1,9,7]
a1 = sorted(a)
print(a1)


# In[10]:


# 문자 정렬
b = 'zbdaf'
b1 = sorted(b)
print(b1)


# #### 2. join() 함수로 리스트를 문자열로 결합

# In[11]:


b = 'zbdaf'
b1 = "".join(sorted(b))
print(b1)


# #### 3. sort() 함수로 리스트 자체를 정렬
# - 제자리 정렬(In-place Sort): 입력을 출력으로 덮어 쓰기 때문에 별도의 추가 공간이 필요하지 않고 리턴값이 없음

# In[12]:


# 알파벳 순서대로 정렬하기

c = ['ccc', 'aaaa', 'd', 'bb']
c1 = sorted(c)
print(c1)


# In[13]:


# 정렬을 위한 함수로 길이를 구하는 len을 지정
# → 알파벳 순서가 아닌 길이 순서로 정렬됨

c = ['ccc', 'aaaa', 'd', 'bb']
c1 = sorted(c, key=len)
print(c1)


# In[14]:


# 함수로 첫 문자열과 마지막 문자열 순으로 정렬(두 번째 키로 마지막 문자를 보게 한 것)
# 첫 문자열: (s[0]), 마지막 문자열: (s[-1])

a = ['cde', 'cfc', 'abc']

def fn(s):
    return s[0], s[-1]

print(sorted(a, key=fn))


# #### 4. 람다를 이용하여 정렬 처리

# In[15]:


a = ['cde', 'cfc', 'abc']
sorted(a, key=lambda s: (s[0], s[-1]))


# ## 06. 가장 긴 팰린드롬 부분 문자열
# Q. 가장 긴 팰린드롬 부분 문자열을 출력하라.  
# 
# **- 입력**  
# 예제 1: "babad"  
# 예제 2: "cbbd"

# ### 풀이 1. 중앙을 중심으로 확장하는 풀이
# - 최장 공통 부분 문자열(Longest Common Substring)
#   - 여러 개의 입력 문자열이 있을 때, 서로 공통된 가장 긴 부분 문자열을 찾는 문제

# In[16]:


class Solution:
    def longestPalindrome(self, s: str) -> str:
        
        # 팰린드롬 판별 및 투 포인터 확장
        def expand(left: int, right: int) -> str:
                
            #left가 0보다 크고 right가 글자 수보다 작거나 같고 s[left] == s[오른쪽-1]이면 반복
            #s[left] == s[ringt-1]: 짝수 expand는 "bb", 홀수 expand는 "bab"를 찾기 위함
            while left >= 0 and right < len(s) and s[left] == s[right]:
                
                #슬라이싱은 n-1 위치 출력, 인덱스는 n 위치 출력
                left -= 1
                right += 1
            
            #while문에서 팰린드롬을 찾았을 때 -1 했으므로 반대로 해주는 것
            return s[left + 1:right]

        # 해당 사항이 없을때 빠르게 리턴
        if len(s) < 2 or s == s[::-1]:
            return s
        
        result = ''
        
        #슬라이딩 윈도우 우측으로 이동
        #제일 긴 펠린드롬을 result에 저장하고 더 긴것을 찾으면 갱신
        #max( key=len) 필수, 글자수를 기준으로 max값 선별
        for i in range(len(s) - 1):
            result = max(result,
                         expand(i, i + 1), #짝수 투포인터
                         expand(i, i + 2), #홀수 투포인터
                         key=len)
        return result

if __name__ == '__main__':
  s = Solution()
  print(s.longestPalindrome("babad"))
  print('\n',s.longestPalindrome("cbbd"))
  print('\n',s.longestPalindrome("gfioabaoidt"))


# In[ ]:




